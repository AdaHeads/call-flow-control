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
with Ada.Strings.Unbounded.Equal_Case_Insensitive;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;

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
      return Call_URI_Prefix & Image (Object.Peer);
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

   function Create (User_ID : in Identifications;
                    Object  : GNATCOLL.JSON.JSON_Value) return Instance is
      Peer_ID : Model.Peer.Identification renames
        Object.Get (Extension_String);
   begin

      return (ID            => User_ID,
              WebSocket     => <>,
              Attributes    => Object,
              Current_State => <>,
              Peer          => Peer_ID);
   end Create;

   --------------
   --  Create  --
   --------------

   function Create (User_ID : in Identifications;
                    Object  : GNATCOLL.JSON.JSON_Value) return Reference is
   begin
      return New_Object : Reference do
         New_Object     := new Instance;
         New_Object.all := Create (User_ID => User_ID,
                                   Object  => Object);
      end return;
   end Create;

   --------------
   --  Create  --
   --------------

   function Create (Object  : GNATCOLL.JSON.JSON_Value) return Instance is
      ID : constant Model.User.Identifications :=
        Natural'(Object.Get (ID_String));
   begin
      return Create (User_ID => ID,
                     Object  => Object);
   end Create;

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
      return Identifications'Pos (Identification);
   end Hash;

   ----------------------
   --  Identification  --
   ----------------------

   function Identification (Object : in Instance) return Identifications is
   begin
      return Object.ID;
   end Identification;

   -------------------
   --  Identity_Of  --
   -------------------

   function Identity_Of (Item : Unbounded_String) return Identities is
   begin
      return Identities (Item);
   end Identity_Of;

   -------------
   --  Image  --
   -------------

   function Image (Object : in Instance) return String is
   begin
      return Image (Object.ID);
   end Image;

   -------------
   --  Image  --
   -------------

   function Image (Identity : Identities) return String is
   begin
      return To_String (Identity);
   end Image;

   -------------
   --  Image  --
   -------------

   function Image (Identification : Identifications) return String is
      Untrimmed_Image : String renames Identifications'Image (Identification);
   begin
      return Ada.Strings.Fixed.Trim (Source => Untrimmed_Image,
                                     Side   => Ada.Strings.Both);
   end Image;

   --------------
   --  Key_Of  --
   --------------

   function Key_Of (Item : Identities) return Unbounded_String is
   begin
      return Item;
   end Key_Of;

   function No_User return Instance is
   begin
      return (ID            => <>,
              WebSocket     => <>,
              Attributes    => <>,
              Current_State => <>,
              Peer          => <>);
   end No_User;

   -------------------------------
   --   Parking_Lot_Identifier  --
   -------------------------------

   function Parking_Lot_Identifier (Object : in Instance) return String is
   begin
      return Parking_Lot_Prefix & Image (Object.ID);
   end Parking_Lot_Identifier;

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
      JSON.Set_Field (ID_String, Image (Object.ID));
      JSON.Set_Field (User_String, Object.Attributes);
      JSON.Set_Field (Name_String,
                      String'(Object.Attributes.Get (Name_String)));
      JSON.Set_Field ("current_state", To_Lower (Object.Current_State'Img));
      if Object.Peer /= Model.Peer.Null_Identification then
         JSON.Set_Field
           (Peer_String,
            Model.Peer.List.Get_Singleton.Get
              (Identity => Object.Peer).To_JSON);
      else
         JSON.Set_Field (Peer_String, Create);
      end if;

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

   -----------------
   --  WebSocket  --
   -----------------

   function WebSocket (User : in Instance) return
     Handlers.Notifications.Object is
   begin
      return User.WebSocket;
   end WebSocket;

end Model.User;
