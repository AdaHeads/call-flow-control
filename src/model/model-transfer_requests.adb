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

with
  Ada.Containers.Hashed_Sets;
with
  Ada_2012.Strings.Equal_Case_Insensitive,
  Ada_2012.Strings.Hash_Case_Insensitive,
  System_Messages;

package body Model.Transfer_Requests is
   use Ada.Containers;

   function Equivalent_Elements (Left, Right : in Request_Tuple)
                                 return Boolean;

   function Hash (IDs : in Request_Tuple) return Hash_Type;

   package Request_Storage is new Ada.Containers.Hashed_Sets
     (Element_Type        => Request_Tuple,
      Hash                => Hash,
      Equivalent_Elements => Equivalent_Elements,
      "="                 => Equivalent_Elements);

   Requests : Request_Storage.Set := Request_Storage.Empty_Set;

   ---------------
   --  Confirm  --
   ---------------

   procedure Confirm (IDs : in Request_Tuple) is
      Context : constant String := Package_Name & ".Confirm";
   begin
      System_Messages.Debug (Message => "Confirmed and removed " & Image (IDs),
                             Context => Context);
      Requests.Delete (Item => IDs);
   end Confirm;

   --------------
   --  Create  --
   --------------

   procedure Create (IDs : in Request_Tuple) is
      use type Model.Call.Identification;
      Context : constant String := Package_Name & ".Create";
   begin
      if IDs.ID1 = IDs.ID2 then
         raise Constraint_Error with "Cannot transfer to own channel!";
      end if;

      if
        Requests.Contains (Item => (ID1 => IDs.ID1,
                                    ID2 => IDs.ID2))
      or
        Requests.Contains (Item => (ID1 => IDs.ID2,
                                    ID2 => IDs.ID1))
      then
         raise Constraint_Error with
           "Cannot perform multiple simultaneous "&
           "transfers on a single channel!";
      end if;

      System_Messages.Debug (Message => "Created " & Image (IDs),
                             Context => Context);
      Requests.Insert (New_Item => IDs);
   end Create;

   ---------------
   --  Decline  --
   ---------------

   procedure Decline (IDs : in Request_Tuple) is
      Context : constant String := Package_Name & ".Decline";
   begin
      System_Messages.Debug (Message => "Declined  and removed " & Image (IDs),
                             Context => Context);
      null;
      --  TODO: Send out confirmation notice on websocket.

   end Decline;

   ---------------------------
   --  Equivalent_Elements  --
   ---------------------------

   function Equivalent_Elements (Left, Right : in Request_Tuple)
                                 return Boolean is
   begin
      return Ada_2012.Strings.Equal_Case_Insensitive (Left  => Image (Left),
                                                      Right => Image (Right));
   end Equivalent_Elements;

   ------------
   --  Hash  --
   ------------

   function Hash (IDs : in Request_Tuple) return Hash_Type is
   begin
      return Ada_2012.Strings.Hash_Case_Insensitive (IDs.ID1.Image);
   end Hash;

   -------------
   --  Image  --
   -------------

   function Image (IDs : in Request_Tuple) return String is
   begin
      return "(" & IDs.ID1.Image & ", " & IDs.ID2.Image & ")";
   end Image;

   ---------------------------
   --  Is_Transfer_Request  --
   ---------------------------

   function Is_Transfer_Request (IDs : in Request_Tuple)
                                 return Boolean is
   begin
      return
        Requests.Contains (Item => (ID1 => IDs.ID1,
                                    ID2 => IDs.ID2))
      or
        Requests.Contains (Item => (ID1 => IDs.ID2,
                                    ID2 => IDs.ID1));
   end Is_Transfer_Request;

end Model.Transfer_Requests;
