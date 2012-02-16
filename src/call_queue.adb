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

with Ada.Calendar;
with Ada.Calendar.Conversions;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Containers.Hashed_Maps;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with AWS.Utils;
with GNATCOLL.JSON;
with Task_Controller;
with Yolk.Utilities;

package body Call_Queue is

   use Ada.Calendar;
   use Ada.Containers;
   use Ada.Numerics;
   use Ada.Strings.Unbounded;
   use GNATCOLL.JSON;
   use Task_Controller;
   use Yolk.Utilities;

   type Priority_Level is (Low, Normal, High);

   package Random_Priority is new Discrete_Random (Priority_Level);
   use Random_Priority;
   --  This is used by the FreeSWITCH_Queue_Monitor task to generate random
   --  calls.

   subtype Call_Id is String (1 .. 10);

   type Call is
      record
         Id       : Call_Id;
         Callee   : String (1 .. 8);
         Caller   : String (1 .. 8);
         Priority : Priority_Level;
         Start    : Ada.Calendar.Time;
      end record;

   --     procedure Remove
   --       (A_Call : in Call);

   function Equal_Elements
     (Left, Right : in Call)
      return Boolean;

   function Equal_Keys
     (Left, Right : in Call_Id)
      return Boolean;

   package Call_Queue_Map is new Hashed_Maps
     (Key_Type        => Call_Id,
      Element_Type    => Call,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => Equal_Keys,
      "="             => Equal_Elements);

   use Call_Queue_Map;

   protected Queue is
      procedure Add
        (Id         : in Call_Id;
         Callee     : in String;
         Caller     : in String;
         Priority   : in Priority_Level;
         Start      : in Ada.Calendar.Time);
      --  Add a waiting call to the call queue.

      procedure Build_JSON;

      procedure Clear;

      function Get
        return String;
      --  Return the call queue JSON string.

      function Length
        return Natural;

      procedure Remove
        (Id : in Call_Id);
      --  Remove a call from the queue.
   private
      JSON_Needs_Building : Boolean := True;
      JSON_Object         : JSON_Value := JSON_Null;
      JSON_String         : Unbounded_String := TUS ("{}");
      Low_JSON_Arr        : JSON_Array := Empty_Array;
      Normal_JSON_Arr     : JSON_Array := Empty_Array;
      High_JSON_Arr       : JSON_Array := Empty_Array;
      Queue_Map           : Map;
   end Queue;

   task FreeSWITCH_Queue_Monitor;
   --  This is supposed to monitor one or more FreeSWITCH servers. Currently
   --  it just generates dummy calls.

   task body FreeSWITCH_Queue_Monitor
   is
      type Foo is mod 50;
      Id_Array : array (Foo) of Call_Id := (others => "          ");
      C        : Foo := 0;

      G : Generator;
      P : Priority_Level;
   begin
      Reset (G);

      loop
         exit when Task_State = Down;

         P := Random (G);

         Id_Array (C) := AWS.Utils.Random_String (10);

         Queue.Add (Id       => Id_Array (C),
                    Callee   => AWS.Utils.Random_String (8),
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
   end FreeSWITCH_Queue_Monitor;

   task Generate_JSON;
   --  Rebuild the queue JSON.

   task body Generate_JSON
   is
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
     return Natural
   is
   begin
      return Queue.Length;
   end Length;

   --------------
   --  Remove  --
   --------------

   --     procedure Remove
   --       (A_Call : in Call)
   --     is
   --     begin
   --        Queue.Remove (A_Call);
   --     end Remove;

   -------------
   --  Queue  --
   -------------

   protected body Queue is

      -----------
      --  Add  --
      -----------

      procedure Add
        (Id       : in Call_Id;
         Callee   : in String;
         Caller   : in String;
         Priority : in Priority_Level;
         Start    : in Ada.Calendar.Time)
      is
      begin
         Queue_Map.Insert (Key      => Id,
                           New_Item => (Id       => Id,
                                        Callee   => Callee,
                                        Caller   => Caller,
                                        Priority => Priority,
                                        Start    => Start));
         JSON_Needs_Building := True;
      end Add;

      -----------
      --  Add  --
      -----------

      --        procedure Add
      --          (A_Call : in Call)
      --        is
      --           function JSONIFY
      --             (A_Call : in Call)
      --              return JSON_Value;
      --           --  Turn an A_Call record into a JSON object.
      --
      --           function JSONIFY
      --             (A_Call : in Call)
      --              return JSON_Value
      --           is
      --              use Ada.Calendar.Formatting;
      --
      --              V : constant JSON_Value := Create_Object;
      --           begin
      --              V.Set_Field ("id", A_Call.Id);
      --              V.Set_Field ("callee", A_Call.Callee);
      --              V.Set_Field ("caller", A_Call.Caller);
      --      V.Set_Field ("priority", Priority_Level'Image (A_Call.Priority));
      --              V.Set_Field ("start", Image (A_Call.Start));
      --
      --              return V;
      --           end JSONIFY;
      --        begin
      --           case A_Call.Priority is
      --              when Low =>
      --                 --  Queue_Low.Insert (A_Call);
      --                 Append (Arr => Low_JSON_Arr,
      --                         Val => JSONIFY (A_Call));
      --                 JSON_Object.Set_Field ("low", Low_JSON_Arr);
      --              when Normal =>
      --                 --  Queue_Normal.Insert (A_Call);
      --                 Append (Arr => Normal_JSON_Arr,
      --                         Val => JSONIFY (A_Call));
      --                 JSON_Object.Set_Field ("normal", Normal_JSON_Arr);
      --              when High =>
      --                 --  Queue_High.Insert (A_Call);
      --                 Append (Arr => High_JSON_Arr,
      --                         Val => JSONIFY (A_Call));
      --                 JSON_Object.Set_Field ("high", High_JSON_Arr);
      --           end case;
      --
      --           JSON_String := TUS (Write (JSON_Object));
      --        end Add;

      ------------------
      --  Build_JSON  --
      ------------------

      procedure Build_JSON
      is
         use Ada.Calendar.Conversions;
         use Ada.Calendar.Formatting;
         use Ada.Calendar.Time_Zones;
         use Ada.Strings.Fixed;
         use Ada.Strings.Maps.Constants;

         procedure Go
           (Position : in Cursor);

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
            Value.Set_Field
              ("priority", Translate
                 (Priority_Level'Image (A_Call.Priority),
                  Lower_Case_Map));
            Value.Set_Field ("start", Image
              (Date      => A_Call.Start,
               Time_Zone => UTC_Time_Offset (A_Call.Start)));
            Value.Set_Field ("stamp", To_Unix_Time (A_Call.Start));

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
      begin
         if JSON_Needs_Building then
            JSON_Object := Create_Object;
            Low_JSON_Arr    := Empty_Array;
            Normal_JSON_Arr := Empty_Array;
            High_JSON_Arr   := Empty_Array;

            Queue_Map.Iterate (Go'Access);

            JSON_Object.Set_Field ("low", Low_JSON_Arr);
            JSON_Object.Set_Field ("normal", Normal_JSON_Arr);
            JSON_Object.Set_Field ("high", High_JSON_Arr);

            JSON_String := TUS (Write (JSON_Object));
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
