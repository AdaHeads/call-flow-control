-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
--                     Author: Kim Rostgaard Christensen                     --
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

with Ada.Containers.Hashed_Sets,
     Ada.Strings.Equal_Case_Insensitive,
     Ada.Strings.Hash_Case_Insensitive;

with System_Messages,
     Model.Call;

package body Model.Origination_Requests is
   use Ada.Containers;

   function Equivalent_Elements (Left, Right : in Model.Call.Identification)
                                 return Boolean;

   function Hash (ID : in Model.Call.Identification) return Hash_Type;

   package Request_Storage is new Ada.Containers.Hashed_Sets
     (Element_Type        => Model.Call.Identification,
      Hash                => Hash,
      Equivalent_Elements => Equivalent_Elements,
      "="                 => Equivalent_Elements);

   Requests : Request_Storage.Set := Request_Storage.Empty_Set;

   ---------------
   --  Confirm  --
   ---------------

   procedure Confirm (ID : in Model.Call.Identification) is
      Context : constant String := Package_Name & ".Confirm";
   begin
      System_Messages.Debug (Message => "Confirmed and removed " & ID.Image,
                             Context => Context);
      Requests.Delete (Item => ID);
      Model.Call.Get (Call => ID).Mark_As_Call;
   end Confirm;

   --------------
   --  Create  --
   --------------

   procedure Create (ID : in Model.Call.Identification) is
      Context : constant String := Package_Name & ".Create";
   begin
      System_Messages.Debug (Message => "Created " & ID.Image,
                             Context => Context);
      Requests.Insert (New_Item => ID);
   end Create;

   ---------------
   --  Decline  --
   ---------------

   procedure Decline (ID : in Model.Call.Identification) is
      Context : constant String := Package_Name & ".Decline";
   begin
      System_Messages.Debug (Message => "Declined " & ID.Image,
                             Context => Context);
      null;
      --  TODO: Send out confirmation notice on websocket.

   end Decline;

   ---------------------------
   --  Equivalent_Elements  --
   ---------------------------

   function Equivalent_Elements (Left, Right : in Model.Call.Identification)
                                 return Boolean is
   begin
      return Ada.Strings.Equal_Case_Insensitive (Left  => Left.Image,
                                                 Right => Right.Image);
   end Equivalent_Elements;

   ------------
   --  Hash  --
   ------------

   function Hash (ID : in Model.Call.Identification) return Hash_Type is
   begin
      return Ada.Strings.Hash_Case_Insensitive (ID.Image);
   end Hash;

   ------------------------------
   --  Is_Origination_Request  --
   ------------------------------

   function Is_Origination_Request (ID : in Model.Call.Identification)
                                    return Boolean is
   begin
      return Requests.Contains (ID);
   end Is_Origination_Request;

end Model.Origination_Requests;
