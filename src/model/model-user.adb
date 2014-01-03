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

with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;

with Model.Peer.List;

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
      return Ada.Strings.Unbounded.Equal_Case_Insensitive
        (Left  => Left,
         Right => Right);

   end "=";

   ---------------------
   --  Authenticated  --
   ---------------------

   function Authenticated (Object : in Instance) return Boolean is
   begin
      return Object.Permissions /= No_Permissions;
   end Authenticated;

   ----------------
   --  Call_URI  --
   ----------------

   function Call_URI (Object : in Instance) return String is
   begin
      return Call_URI_Prefix & Model.Peer.List.Get_Singleton.Get
        (Identity => Object.Peer).Get_Identification;
   end Call_URI;

   --------------------
   --  Change_State  --
   --------------------

   procedure Change_State (Object    :    out Instance;
                           New_State : in     States) is
   begin
      Object.Current_State := New_State;
   end Change_State;

   --------------
   --  Create  --
   --------------

   function Create (ID     : in Identities;
                    Object : GNATCOLL.JSON.JSON_Value) return Instance is
      Peer_ID : constant Model.Peer.Identification := Object.Get ("extension");
   begin

      return (ID            => ID,
              Attributes    => Object,
              Current_State => <>,
              Current_Call  => <>,
              Peer          => Peer_ID);
   end Create;

   function Create (ID     : in Identities;
                    Object : GNATCOLL.JSON.JSON_Value) return Reference is
   begin
      return New_Object : Reference do
         New_Object     := new Instance;
         New_Object.all := Create (ID     => ID,
                                   Object => Object);
      end return;
   end Create;

   --------------------
   --  Current_Call  --
   --------------------

   function Current_Call (Object : in Instance)
                          return PBX.Call.Instance is
   begin
      return PBX.Call.Get (Object.Current_Call);
   end Current_Call;

   ---------------------
   --  Current_State  --
   ---------------------

   function Current_State (Object : in Instance) return States is
   begin
      return Object.Current_State;
   end Current_State;

   -----------
   --  Hash --
   -----------

   function Hash (Identity : Identities) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Unbounded.Hash_Case_Insensitive (Key => Identity);
   end Hash;

   -----------
   --  Hash --
   -----------

   function Hash (Identification : Identifications)
                  return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Identification);
   end Hash;

   ----------------------
   --  Identification  --
   ----------------------

   function Identification (Object : in Instance) return Identifications is
   begin
      return Identifications
        (Natural'(Object.Attributes.Get (Field => ID_String)));
   end Identification;

   ----------------------
   --  Identification  --
   ----------------------

   function Identification (Object : in Instance) return String is
   begin
      return Object.Attributes.Get (Field => ID_String);
   end Identification;

   ----------------
   --  Identity  --
   ----------------

   function Identity (Object : in Instance) return Identities is
   begin
      return Identities (Object.ID);
   end Identity;

   -------------------
   --  Identity_Of  --
   -------------------

   function Identity_Of (Item : Unbounded_String) return Identities is
   begin
      return Identities (Item);
   end Identity_Of;

   function Image (Object : in Instance) return String is
      User_ID : Natural renames Object.Attributes.Get (Field => ID_String);
   begin
      return To_String (Object.Identity) & " (ID:" & User_ID'Img & ")";
   end Image;

   --------------
   --  Key_Of  --
   --------------

   function Key_Of (Item : Identities) return Unbounded_String is
   begin
      return Item;
   end Key_Of;

   ------------
   --  Peer  --
   ------------

   function Peer (Object : in Instance) return Model.Peer.Instance is
   begin
      return Model.Peer.List.Get_Singleton.Get (Object.Peer);
   end Peer;

   -------------------
   --  Permissions  --
   -------------------

   function Permissions (User : in Instance) return Permission_List is

      Context : constant String := Package_Name & ".Permissions";
      pragma Unreferenced (Context);

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

      function To_Lower (Item : in String) return String;

      function To_Lower (Item : in String) return String is
      begin
         return Ada.Strings.Fixed.Translate
           (Source  => Item,
            Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
      end To_Lower;

   begin
      JSON.Set_Field ("identity", To_String (Object.ID));
      JSON.Set_Field ("user", Object.Attributes);
      JSON.Set_Field ("current_state", To_Lower (Object.Current_State'Img));
      JSON.Set_Field ("current_call", Object.Current_Call.Image);
      JSON.Set_Field
        ("peer",
         Model.Peer.List.Get_Singleton.Get
           (Identity => Object.Peer).To_JSON);

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
      return To_Unbounded_String (Item);
   end Value;

end Model.User;
