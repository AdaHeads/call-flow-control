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

with Ada.Calendar.Formatting;
with Ada.Containers.Ordered_Sets;
with GNATCOLL.JSON;
with Yolk.Utilities;

package body Call_Queue is

   use Ada.Containers;
   use GNATCOLL.JSON;
   use Yolk.Utilities;

   function Equal
     (Left, Right : in Call)
      return Boolean;

   function Lesser_Than
     (Left, Right : in Call)
      return Boolean;

   package Call_Queue_Set is new Ordered_Sets
     (Element_Type => Call,
      "<"          => Lesser_Than,
      "="          => Equal);

   use Call_Queue_Set;

   protected Queue is
      procedure Add
        (A_Call : in Call);
      --  Add a waiting call to the call queue.

      function Get
        return String;
      --  Return the call queue JSON string.

      procedure Remove
        (A_Call : in Call);
      --  Remove a call from the queue.
   private
      JSON_Object : JSON_Value := Create_Object;
      JSON_String : Unbounded_String := TUS ("{}");

      Low_JSON_Arr    : JSON_Array := Empty_Array;
      Normal_JSON_Arr : JSON_Array := Empty_Array;
      High_JSON_Arr   : JSON_Array := Empty_Array;

      Queue_Low    : Set;
      Queue_Normal : Set;
      Queue_High   : Set;
   end Queue;

   -----------
   --  Add  --
   -----------

   procedure Add
     (Id       : in Call_Id;
      Callee   : in String;
      Caller   : in String;
      Priority : in Priority_Level := Normal;
      Start    : in Ada.Calendar.Time)
   is
   begin
      Queue.Add ((Id       => Id,
                  Callee   => TUS (Callee),
                  Caller   => TUS (Caller),
                  Priority => Priority,
                  Start    => Start));
   end Add;

   -------------
   --  Equal  --
   -------------

   function Equal
     (Left, Right : in Call)
      return Boolean
   is
   begin
      return Left.Id = Right.Id;
   end Equal;

   -----------
   --  Get  --
   -----------

   function Get
     return String
   is
   begin
      return Queue.Get;
   end Get;

   -------------------
   --  Lesser_Than  --
   -------------------

   function Lesser_Than
     (Left, Right : in Call)
      return Boolean
   is
      use Ada.Calendar;
   begin
      return Left.Start <= Right.Start;
   end Lesser_Than;

   --------------
   --  Remove  --
   --------------

   procedure Remove
     (A_Call : in Call)
   is
   begin
      Queue.Remove (A_Call);
   end Remove;

   -------------
   --  Queue  --
   -------------

   protected body Queue is
      -----------
      --  Add  --
      -----------

      procedure Add
        (A_Call : in Call)
      is
         function JSONIFY
           (A_Call : in Call)
            return JSON_Value;
         --  Turn an A_Call record into a JSON object.

         function JSONIFY
           (A_Call : in Call)
            return JSON_Value
         is
            use Ada.Calendar.Formatting;

            V : constant JSON_Value := Create_Object;
         begin
            V.Set_Field ("id", A_Call.Id);
            V.Set_Field ("callee", A_Call.Callee);
            V.Set_Field ("caller", A_Call.Caller);
            V.Set_Field ("priority", Priority_Level'Image (A_Call.Priority));
            V.Set_Field ("start", Image (A_Call.Start));

            return V;
         end JSONIFY;
      begin
         case A_Call.Priority is
            when Low =>
               Queue_Low.Insert (A_Call);
               Append (Arr => Low_JSON_Arr,
                       Val => JSONIFY (A_Call));
               JSON_Object.Set_Field ("low", Low_JSON_Arr);
            when Normal =>
               Queue_Normal.Insert (A_Call);
               Append (Arr => Normal_JSON_Arr,
                       Val => JSONIFY (A_Call));
               JSON_Object.Set_Field ("normal", Normal_JSON_Arr);
            when High =>
               Queue_High.Insert (A_Call);
               Append (Arr => High_JSON_Arr,
                       Val => JSONIFY (A_Call));
               JSON_Object.Set_Field ("high", High_JSON_Arr);
         end case;

         JSON_String := TUS (Write (JSON_Object));
      end Add;

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
      --  Remove  --
      --------------

      procedure Remove
        (A_Call : in Call)
      is
      begin
         case A_Call.Priority is
            when Low =>
               Queue_Low.Exclude (A_Call);
            when Normal =>
               Queue_Normal.Exclude (A_Call);
            when High =>
               Queue_High.Exclude (A_Call);
         end case;
      end Remove;
   end Queue;

end Call_Queue;
