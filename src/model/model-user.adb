-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2013-, AdaHeads K/S                     --
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

with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;

with System_Messages;

package body Model.User is

   -----------
   --  "<"  --
   -----------

   function "<" (Left, Right : in Instance) return Boolean is
   begin
      return Left.ID < Right.ID;
   end "<";

   -----------
   --  "="  --
   -----------

   function "=" (Left, Right : in Instance) return Boolean is
   begin
      return Left.ID = Right.ID;
   end "=";

   -----------
   --  "="  --
   -----------

   function "=" (Left, Right : in Identities) return Boolean is
   begin
      return Ada.Strings.Equal_Case_Insensitive
        (Left  => String (Left),
         Right => String (Right));

   end "=";

   ---------------------
   --  Authenticated  --
   ---------------------

   function Authenticated (Object : in Instance) return Boolean is
   begin
      return Object.Permissions /= No_Permissions;
   end Authenticated;

   --------------
   --  Create  --
   --------------

   function Create (ID     : in Identities;
                    Object : GNATCOLL.JSON.JSON_Value) return Instance is
   begin

      return (ID => To_Unbounded_String (String (ID)), Attributes => Object);
   end Create;

   -----------
   --  Hash --
   -----------

   function Hash (Identity : Identities) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash_Case_Insensitive (Key => String (Identity));
   end Hash;

   -----------
   --  Hash --
   -----------

   function Hash (Identification : Identifications)
                  return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Identification);
   end Hash;

   ----------------
   --  Identity  --
   ----------------

   function Identity (Object : in Instance) return Identities is
   begin
      return Identities (To_String (Object.ID));
   end Identity;

   -------------------
   --  Identity_Of  --
   -------------------

   function Identity_Of (Item : Unbounded_String) return Identities is
   begin
      return Identities (To_String (Item));
   end Identity_Of;

   --------------
   --  Key_Of  --
   --------------

   function Key_Of (Item : Identities) return Unbounded_String is
   begin
      return To_Unbounded_String (String (Item));
   end Key_Of;

   -------------------
   --  Permissions  --
   -------------------

   function Permissions (User : in Instance) return Permission_List is

      Context : constant String := Package_Name & ".Permissions";

      Result   : Permission_List := (others => False);
      Perm_Arr : JSON_Array;
   begin

      if User = No_User then
         return Result;
      end if;

      Perm_Arr := User.Attributes.Get (Groups_String);

      for I in 1 .. Length (Perm_Arr) loop
         declare
            Node : constant JSON_Value := Get (Perm_Arr, I);
         begin
            if Get (Node) = Receptionist_String then
               Result (Receptionist) := True;
            elsif Get (Node) = Administrator_String then
               Result (Administrator) := True;
            elsif Get (Node) = Service_Agent_String then
               Result (Service_Agent) := True;
            end if;
         end;
      end loop;

      return Result;
   end Permissions;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON (Object : in Instance) return JSON_Value is
      JSON : constant JSON_Value := Create_Object;
   begin
      JSON.Set_Field ("identity", To_String (Object.ID));
      JSON.Set_Field ("user", Object.Attributes);

      return JSON;
   end To_JSON;

   -------------
   --  Value  --
   -------------

   function Value (Item : in String) return Identifications is
   begin
      return Identifications'Value (Item);
   end Value;

   -------------
   --  Value  --
   -------------

   function Value (Item : in String) return Identities is
   begin
      return Identities (Item);
   end Value;

end Model.User;
