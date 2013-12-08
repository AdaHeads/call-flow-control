-------------------------------------------------------------------------------
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

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with System_Messages; use System_Messages;

package body Model.Draft_Stack is
   use Ada.Containers;

   package Draft_Stack_Storage is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => JSON_Value,
      "="          => "=");

   package Agent_Stack_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Model.Agent_ID.Agent_ID_Type,
      Element_Type    => Draft_Stack_Storage.Vector,
      Hash            => Model.Agent_ID.Hash,
      Equivalent_Keys => Model.Agent_ID."=",
      "="             => Draft_Stack_Storage."=");

   Draft_Stacks : Agent_Stack_Storage.Map := Agent_Stack_Storage.Empty_Map;

   function Stack_Of (Agent : Model.Agent_ID.Agent_ID_Type) return Instance is
      Obj : Instance;
   begin
      Obj.Identifier := Agent;
      return Obj;
   end Stack_Of;

   function To_JSON (Object : in Instance) return JSON_Value is
      Value     : JSON_Value := Create_Object;
      JSON_List : JSON_Array;
      Root      : constant JSON_Value := Create_Object;
   begin
      for Draft of Draft_Stacks.Element (Object.Identifier) loop
         Append (JSON_List, Draft);
      end loop;
      Root.Set_Field ("agent_id", Object.Identifier.ID);
      Root.Set_Field ("drafts", JSON_List);
      return Root;
   end To_JSON;

   procedure Delete (Object : in Instance;
                     Id     : in Draft_ID) is
   begin
      raise Program_Error with "Model.Draft_Stack.Delete - not implemented";
   end Delete;

   function Push (Object : in Instance;
                  Draft  : in JSON_Value) return Draft_ID is
   begin
      return 0;
   end Push;
begin
   --  Dummy code.
   for I in 1 .. 20 loop
      declare
         Root   : constant JSON_Value := Create_Object;
         Vector : Draft_Stack_Storage.Vector :=
           Draft_Stack_Storage.Empty_Vector;
      begin
         Root.Set_Field ("id", I);
         Root.Set_Field ("subject", "urgent");
         Root.Set_Field ("body", "hat");
         Vector.Append (New_Item => Root);
         Draft_Stacks.Insert (Key      => Model.Agent_ID.Create (I),
                              New_Item => Vector);
      end;
   end loop;

   for I in 1 .. 20 loop
      System_Messages.Notify
        (Level   => Debug,
         Message => "DRAFT:" &
           Stack_Of (Model.Agent_ID.Create (I)).To_JSON.Write);
   end loop;
end Model.Draft_Stack;
