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

with Ada.Text_IO;
with Alice_Configuration;
with System_Messages;

package body Model.User.List is
   use Ada.Text_IO;
   use Ada.Containers;
   use Alice_Configuration;

   Users : Instance;

   -----------
   --  Get  --
   -----------

--     function Get (Object  : in Instance;
--                   User_ID : in User.Identifications) return User.Instance is
--     begin
--        return Object.User_Map.Element (Object.ID_Lookup_Map.Element (User_ID));
--     end Get;

   -----------
   --  Get  --
   function Get (Object   : in Instance;
                 Identity : in User.Identities) return User.Instance is
   begin
      if not Object.User_Map.Contains (User.Key_Of (Identity)) then
         return No_User;
      end if;

      return Object.User_Map.Element (User.Key_Of (Identity));
   end Get;

   ---------------------
   --  Get_Singleton  --
   ---------------------

   function Get_Singleton return Instance is
   begin
      return Users;
   end Get_Singleton;

   ------------------
   --  Reload_Map  --
   ------------------

   procedure Reload_Map (Object    :    out Instance;
                         Filename : in     String) is
      use System_Messages;

      Context : constant String := Package_Name & ".Reload_Map";

      File         : File_Type;
      In_JSON      : JSON_Value;
      User_Arr     : JSON_Array;
      New_Instance : User.List.Instance;
   begin
      Open (File => File, Mode => In_File,
            Name => Filename);
      In_JSON := GNATCOLL.JSON.Read (Strm => Get_Line (File));
      User_Arr := In_JSON.Get (User.Identities_String);
      for I in 1 .. Length (User_Arr) loop
         declare
            Node     : constant JSON_Value      := Get (User_Arr, I);
            Identity : constant User.Identities :=
              User.Value (Node.Get (User.Identity_String));
            ID       : Identifications;

         begin

--              ID := Identifications
--                (Integer'(Node.Get (User.Identity_String).Get (User.ID_String)));

            New_Instance.User_Map.Insert
              (Key      => Key_Of (Identity),
               New_Item => Model.User.Create
                 (ID     => Identity,
                  Object => Node.Get (User.User_String)));
--              New_Instance.ID_Lookup_Map.Insert
--                (Key      => ID,
--                 New_Item => Key_Of (Identity));
         end;
      end loop;

      Object := New_Instance;

      Information (Message => "Loaded user information from file " & Filename,
                   Context => Context);
   exception
      when Constraint_Error | Name_Error =>
         Critical (Message => "Failed to load user information from file "
                   & Filename,
                   Context => Context);
         raise;

   end Reload_Map;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON (Object : in Instance) return JSON_Value is
      JSON_List : JSON_Array;
      Root      : constant JSON_Value := Create_Object;
   begin
      for User of Object.User_Map loop
         Append (JSON_List, User.To_JSON);
      end loop;
      Root.Set_Field ("identities", JSON_List);
      return Root;
   end To_JSON;

   ---------------
   --  User_Of  --
   ---------------

   function User_Of (Object  : in Instance;
                     Request : AWS.Status.Data) return User.Instance is
      Context : constant String := Package_Name & ".User_Of";

      Static_User : constant String := "kim.rostgaard@gmail.com";
   begin
      System_Messages.Fixme (Message => "Call to stub function, returning" &
                             " credentials of static user " & Static_User,
                             Context => Context);
      --  This procedure depends on the user layer, and that we have a
      --  Valid token present in the request.

      return Object.Get (Identity => Identities (Static_User));
   end User_Of;
begin

   Reload_Map (Users, Config.Get (User_Map_File));
   --  Initial loading of user data to the internal singleton DB.

end Model.User.List;
