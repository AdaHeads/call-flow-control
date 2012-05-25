-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               Call_Queue                                  --
--                                                                           --
--                                  BODY                                     --
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                     --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Calendar.Conversions;
with Ada.Calendar.Formatting;
with Ada.Containers.Ordered_Maps;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with AWS.Utils;
with Interfaces.C;
with GNATCOLL.JSON;
with Task_Controller;
with Yolk.Utilities;

package body Call_Queue is

   use Ada.Containers;
   use Ada.Numerics;
   use Ada.Strings.Unbounded;
   use GNATCOLL.JSON;
   use Yolk.Utilities;

   type Priority_Level is (Low, Normal, High);

   package Random_Priority is new Discrete_Random (Priority_Level);
   --  HERE FOR TESTING PURPOSES.
   --  This is used by the PBX_Queue_Monitor task to generate random calls.

   subtype Org_Id is Natural range 1 .. 5;
   package Random_Organization is new Discrete_Random (Org_Id);
   --  HERE FOR TESTING PURPOSES.
   --  This is just to be able to randomnly grab an organization so it can be
   --  added as the callee in a Call record.

   subtype Call_Id is String (1 .. 10);
   --  Unique identifier for each call in the queue. This type will
   --  obviously depend heavily on the kind of ID used internally by the PBX.

   type Call is
      record
         Id            : Call_Id;
         Callee        : Org_Id;
         Caller        : String (1 .. 8);
         Priority      : Priority_Level;
         Timestamp_UTC : Ada.Calendar.Time;
         --  Note that the time is recorded in UTC. No timezone is given.
      end record;

   function Equal_Elements
     (Left, Right : in Call)
      return Boolean;
   --  Return True if two Call elements are equal.

   function Sort_Elements
     (Left, Right : in Ada.Calendar.Time)
      return Boolean;
   --  Return True if Left is < the Right. This sorts the calls in the ordered
   --  queue maps according to the Call.Timestamp_UTC component.

   package Ordered_Call_Queue_Map is new Ordered_Maps
     (Key_Type     => Ada.Calendar.Time,
      Element_Type => Call,
      "<"          => Sort_Elements,
      "="          => Equal_Elements);
   --  All call queues are kept in a Ordered_Call_Queue_Map, with sorting being
   --  done according to the Call.Timestamp_UTC component.

   type Queue_Maps_Array is array (Priority_Level) of
     Ordered_Call_Queue_Map.Map;

   type Queue_JSON_Array is array (Priority_Level) of JSON_Array;

   protected Queue is
      procedure Add
        (Id         : in Call_Id;
         Callee     : in Org_Id;
         Caller     : in String;
         Priority   : in Priority_Level;
         Start      : in Ada.Calendar.Time);
      --  Add a call to the queue designated by Priority.

      procedure Build_JSON;
      --  Build the queue JSON based on the Call records in the queues.

      procedure Clear;
      --  Delete all calls from the queues.

      function Get
        return String;
      --  Return the queue JSON string.

      function Length
        return Natural;
      --  Return the amount of Call records currently in the queues.

      procedure Remove
        (Id : in Call_Id);
      --  Remove a call from the queue.

      procedure Remove
        (Id      : in     Call_Id;
         Org_Id  :    out Natural;
         Success :    out Boolean);
      --  Remove a call from the queue.

      procedure Remove_First
        (Removed_Call_Id :    out Call_Id;
         Removed_Org_Id  :    out Natural);
      --  Remove the first call in the queue. Removed_Call_Id contains the Id
      --  of the removed call and Removed_Org_Id is the Id of the callee.
      --  If there are no calls to remove, an empty string and a 0 is returned.
   private
      JSON_Needs_Building : Boolean := True;
      --  This is set to False whenever a Call is added or removed from the
      --  queues.

      JSON                : JSON_Value := JSON_Null;
      --  This holds the current JSON_Value object from which the
      --  JSON_String is constructed.

      JSON_String         : Unbounded_String := TUS ("{}");
      --  The JSON returned in call to Get.

      JSON_Arrays         : Queue_JSON_Array := (others => Empty_Array);
      --  The JSON arrays containing the calls in the queue according to
      --  their priority.

      Queue_Maps          : Queue_Maps_Array;
      --  The queue maps.
   end Queue;

   task PBX_Queue_Monitor;
   --  This is supposed to monitor one or more PBX's. Currently it just
   --  generates dummy calls.

   task body PBX_Queue_Monitor
   is
      use Ada.Calendar;
      use Task_Controller;

      type Foo is mod 30;

      Id_Array : array (Foo) of Call_Id := (others => "          ");
      --  Simulate up to a total of 30 calls in the queues.

      C     : Foo := 0;
      --  Basic counter variable.

      G     : Random_Priority.Generator;
      Org_G : Random_Organization.Generator;
      OID   : Org_Id;
      P     : Priority_Level;
      --  Random priority levels on the generated calls.
   begin
      Random_Priority.Reset (G);
      Random_Organization.Reset (Org_G, 42);

      loop
         exit when Task_State = Down;

         P := Random_Priority.Random (G);
         OID := Random_Organization.Random (Org_G);

         Id_Array (C) := AWS.Utils.Random_String (10);

         Queue.Add (Id       => Id_Array (C),
                    Callee   => OID,
                    Caller   => AWS.Utils.Random_String (8),
                    Priority => P,
                    Start    => Clock);

         if Id_Array (C + 1) /= "          " then
            Queue.Remove (Id_Array (C + 1));
         end if;

         C := C + 1;

         delay 15.0;
      end loop;

      Queue.Clear;
   end PBX_Queue_Monitor;

   task Generate_JSON;
   --  Rebuild the queue JSON. Once every ½ second we check if a call has
   --  been either added or removed from the queue, and if that is the case,
   --  then we re-build the queue JSON.

   task body Generate_JSON
   is
      use Task_Controller;
   begin
      loop
         exit when Task_State = Down;

         Queue.Build_JSON;

         delay 0.5;
      end loop;
   end Generate_JSON;

   ----------------------
   --  Equal_Elements  --
   ----------------------

   function Equal_Elements
     (Left, Right : in Call)
      return Boolean
   is
   begin
      return Left = Right;
   end Equal_Elements;

   -----------
   --  Get  --
   -----------

   function Get
     return String
   is
   begin
      return Queue.Get;
   end Get;

   ----------------
   --  Get_Call  --
   ----------------

   function Get_Call
     (Id : in String)
      return String
   is
      JSON    : constant JSON_Value := Create_Object;
      Org_Id  : Natural := 0;
      Success : Boolean;
   begin
      if Id'Length > 0 then
         Queue.Remove (Id, Org_Id, Success);

         if Success then
            JSON.Set_Field ("id", Id);
            JSON.Set_Field ("org_id", Org_Id);
         end if;
      else
         declare
            CI     : Call_Id;
            Org_Id : Natural;
         begin
            Queue.Remove_First (Removed_Call_Id => CI,
                                Removed_Org_Id  => Org_Id);

            if Org_Id > 0 then
               JSON.Set_Field ("id", CI);
               JSON.Set_Field ("org_id", Org_Id);
            end if;
         end;
      end if;

      return JSON.Write;
   end Get_Call;

   --------------
   --  Length  --
   --------------

   function Length
     return String
   is
      JSON : constant JSON_Value := Create_Object;
   begin
      JSON.Set_Field ("length", Queue.Length);

      return JSON.Write;
   end Length;

   -------------
   --  Queue  --
   -------------

   protected body Queue is

      -----------
      --  Add  --
      -----------

      procedure Add
        (Id       : in Call_Id;
         Callee   : in Org_Id;
         Caller   : in String;
         Priority : in Priority_Level;
         Start    : in Ada.Calendar.Time)
      is
      begin
         Queue_Maps (Priority).Insert (Key      => Start,
                                       New_Item => (Id            => Id,
                                                    Callee        => Callee,
                                                    Caller        => Caller,
                                                    Priority      => Priority,
                                                    Timestamp_UTC => Start));

         JSON_Needs_Building := True;
      end Add;

      ------------------
      --  Build_JSON  --
      ------------------

      procedure Build_JSON
      is
         use Ada.Calendar;
         use Ada.Calendar.Conversions;
         use Ada.Calendar.Formatting;
         use Ada.Strings.Fixed;

         procedure Go
           (Position : in Ordered_Call_Queue_Map.Cursor);
         --  JSON'ify each Call in the queue.

         function Unix_Timestamp
           (Date : in Time)
            return String;
         --  Convert and trim an Ada.Calendar.Time type to a Unix timestamp
         --  String.

         ---------
         -- Go  --
         ---------

         procedure Go
           (Position : in Ordered_Call_Queue_Map.Cursor)
         is
            A_Call : Call;
            Value  : constant JSON_Value := Create_Object;
         begin
            A_Call := Ordered_Call_Queue_Map.Element (Position);

            Value.Set_Field ("id", A_Call.Id);
            Value.Set_Field ("callee", A_Call.Callee);
            Value.Set_Field ("caller", A_Call.Caller);
            Value.Set_Field ("UTC_start_date", Image (A_Call.Timestamp_UTC));
            Value.Set_Field
              ("unix_timestamp", Unix_Timestamp (A_Call.Timestamp_UTC));

            Append (JSON_Arrays (A_Call.Priority), Value);
         end Go;

         ----------------------
         --  Unix_Timestamp  --
         ----------------------

         function Unix_Timestamp
           (Date : in Time)
            return String
         is
            use Ada.Strings;
            use Interfaces.C;
         begin
            return Fixed.Trim
              (Source => long'Image (To_Unix_Time (Date)),
               Side   => Left);
         end Unix_Timestamp;
      begin
         if JSON_Needs_Building then
            JSON := Create_Object;

            JSON.Set_Field ("length", Queue.Length);

            JSON_Arrays := (others => Empty_Array);

            for P in Queue_Maps'Range loop
               Queue_Maps (P).Iterate (Go'Access);
            end loop;

            JSON.Set_Field ("low", JSON_Arrays (Low));
            JSON.Set_Field ("normal", JSON_Arrays (Normal));
            JSON.Set_Field ("high", JSON_Arrays (High));

            JSON_String := TUS (JSON.Write);
         end if;
      end Build_JSON;

      -------------
      --  Clear  --
      -------------

      procedure Clear
      is
      begin
         for P in Queue_Maps'Range loop
            Queue_Maps (P).Clear;
         end loop;

         JSON_String := TUS ("{}");
         JSON_Needs_Building := True;
      end Clear;

      -----------
      --  Get  --
      -----------

      function Get
        return String
      is
      begin
         return TS (JSON_String);
      end Get;

      --------------
      --  Length  --
      --------------

      function Length
        return Natural
      is
         C : Natural := 0;
      begin
         for P in Queue_Maps'Range loop
            C := C + Natural (Queue_Maps (P).Length);
         end loop;

         return C;
      end Length;

      --------------
      --  Remove  --
      --------------

      procedure Remove
        (Id : in Call_Id)
      is
         Success : Boolean;
         Org_Id  : Natural;

         pragma Unreferenced (Success);
         pragma Unreferenced (Org_Id);
      begin
         Remove (Id, Org_Id, Success);
      end Remove;

      --------------
      --  Remove  --
      --------------

      procedure Remove
        (Id      : in     Call_Id;
         Org_Id  :    out Natural;
         Success :    out Boolean)
      is
         C    : Ordered_Call_Queue_Map.Cursor;
         Elem : Call;
      begin
         Org_Id := 0;
         Success := False;

         Queue_Maps_Loop :
         for P in Queue_Maps'Range loop
            C := Queue_Maps (P).First;

            Remove_Loop :
            for K in 1 .. Queue_Maps (P).Length loop
               Elem := Ordered_Call_Queue_Map.Element (C);

               if Elem.Id = Id then
                  Org_Id := Elem.Callee;
                  Success := True;
                  JSON_Needs_Building := True;

                  Queue_Maps (P).Delete (C);

                  exit Queue_Maps_Loop;
               end if;

               Ordered_Call_Queue_Map.Next (C);
            end loop Remove_Loop;
         end loop Queue_Maps_Loop;
      end Remove;

      --------------------
      --  Remove_First  --
      --------------------

      procedure Remove_First
        (Removed_Call_Id :    out Call_Id;
         Removed_Org_Id  :    out Natural)
      is
      begin
         if Queue_Maps (High).Length > 0 then
            Removed_Call_Id := Queue_Maps (High).First_Element.Id;
            Removed_Org_Id := Queue_Maps (High).First_Element.Callee;
            Queue_Maps (High).Delete_First;
         elsif Queue_Maps (Normal).Length > 0 then
            Removed_Call_Id := Queue_Maps (Normal).First_Element.Id;
            Removed_Org_Id := Queue_Maps (Normal).First_Element.Callee;
            Queue_Maps (Normal).Delete_First;
         elsif Queue_Maps (Low).Length > 0 then
            Removed_Call_Id := Queue_Maps (Low).First_Element.Id;
            Removed_Org_Id := Queue_Maps (Low).First_Element.Callee;
            Queue_Maps (Low).Delete_First;
         else
            Removed_Call_Id := "          ";
            Removed_Org_Id := 0;
         end if;
      end Remove_First;
   end Queue;

   ---------------------
   --  Sort_Elements  --
   ---------------------

   function Sort_Elements
     (Left, Right : in Ada.Calendar.Time)
      return Boolean
   is
      use Ada.Calendar;
   begin
      return Left < Right;
   end Sort_Elements;

end Call_Queue;
