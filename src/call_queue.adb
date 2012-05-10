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
with Ada.Containers.Hashed_Maps;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
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
   --  This is used by the FreeSWITCH_Queue_Monitor task to generate random
   --  calls.

   subtype Org_Id is Integer range 1 .. 5;
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
   --  Return True if two Call elements are equal, meaning that all the
   --  record components match.

   function Equal_Keys
     (Left, Right : in Call_Id)
      return Boolean;
   --  Return True if the two Call_Id are the same.

   package Call_Queue_Map is new Hashed_Maps
     (Key_Type        => Call_Id,
      Element_Type    => Call,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => Equal_Keys,
      "="             => Equal_Elements);
   --  The hashed map used to hold all the calls. Note that we're not
   --  doing any kind of sorting. This is expected to be done in the
   --  client.

   use Call_Queue_Map;

   protected Queue is
      procedure Add
        (Id         : in Call_Id;
         Callee     : in Org_Id;
         Caller     : in String;
         Priority   : in Priority_Level;
         Start      : in Ada.Calendar.Time);
      --  Add a call to the queue.

      procedure Build_JSON;
      --  Build the queue JSON based on the Call records in the queue.

      procedure Clear;
      --  Delete all calls from the queue.

      function Get
        return String;
      --  Return the queue JSON string.

      function Length
        return Natural;
      --  Return the amount of Call records currently in the queue.

      procedure Remove
        (Id : in Call_Id);
      --  Remove a call from the queue.
   private
      JSON_Needs_Building : Boolean := True;
      --  This is set to False whenever a Call is added or removed from the
      --  queue.

      JSON                : JSON_Value := JSON_Null;
      --  This holds the current JSON_Value object from which the
      --  JSON_String is constructed.

      JSON_String         : Unbounded_String := TUS ("{}");
      --  The JSON returned in call to Get.

      Low_JSON_Arr        : JSON_Array := Empty_Array;
      Normal_JSON_Arr     : JSON_Array := Empty_Array;
      High_JSON_Arr       : JSON_Array := Empty_Array;
      --  The JSON arrays containing the calls in the queue according to
      --  their priority.

      Queue_Map           : Map;
      --  The queue map.
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
      --  Simulate up to 50 calls in the queue.

      C        : Foo := 0;
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

         delay 1.0;
      end loop;

      Queue.Clear;
   end PBX_Queue_Monitor;

   task Generate_JSON;
   --  Rebuild the queue JSON. Once every ½ second we check if a call has
   --  been either added or removed from the queue, and that is the case,
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

   ------------------
   --  Equal_Keys  --
   ------------------

   function Equal_Keys
     (Left, Right : in Call_Id)
      return Boolean
   is
   begin
      return Left = Right;
   end Equal_Keys;

   -----------
   --  Get  --
   -----------

   function Get
     return String
   is
   begin
      return Queue.Get;
   end Get;

   --------------
   --  Length  --
   --------------

   function Length
     return String
   is
      use Ada.Strings;

      JSON : constant JSON_Value := Create_Object;
   begin
      JSON.Set_Field
        ("length",
         Fixed.Trim (Source => Natural'Image (Queue.Length),
                     Side   => Left));
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
         Queue_Map.Insert (Key      => Id,
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
           (Position : in Cursor);
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
           (Position : in Cursor)
         is
            A_Call : Call;
            Value  : constant JSON_Value := Create_Object;
         begin
            A_Call := Element (Position);

            Value.Set_Field ("id", A_Call.Id);
            Value.Set_Field ("callee", A_Call.Callee);
            Value.Set_Field ("caller", A_Call.Caller);
            Value.Set_Field ("UTC_start_date", Image (A_Call.Timestamp_UTC));
            Value.Set_Field
              ("unix_timestamp", Unix_Timestamp (A_Call.Timestamp_UTC));

            case A_Call.Priority is
               when Low =>
                  Append (Arr => Low_JSON_Arr,
                          Val => Value);
               when Normal =>
                  Append (Arr => Normal_JSON_Arr,
                          Val => Value);
               when High =>
                  Append (Arr => High_JSON_Arr,
                          Val => Value);
            end case;
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

            JSON.Set_Field ("length", Natural (Queue_Map.Length));
            Low_JSON_Arr    := Empty_Array;
            Normal_JSON_Arr := Empty_Array;
            High_JSON_Arr   := Empty_Array;

            Queue_Map.Iterate (Go'Access);

            JSON.Set_Field ("low", Low_JSON_Arr);
            JSON.Set_Field ("normal", Normal_JSON_Arr);
            JSON.Set_Field ("high", High_JSON_Arr);

            JSON_String := TUS (JSON.Write);
         end if;
      end Build_JSON;

      -------------
      --  Clear  --
      -------------

      procedure Clear
      is
      begin
         Queue_Map.Clear;
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
      begin
         return Natural (Queue_Map.Length);
      end Length;

      --------------
      --  Remove  --
      --------------

      procedure Remove
        (Id : in Call_Id)
      is
      begin
         Queue_Map.Exclude (Id);
         JSON_Needs_Building := True;
      end Remove;
   end Queue;

end Call_Queue;
