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
with Ada.Exceptions;

with Alice_Configuration;
with System_Messages;

package body Model.User.List is
   use Ada.Text_IO;
   use Ada.Containers;
   use Alice_Configuration;

   Users : aliased Instance := (Path         => Config.Get (User_Map_File),
                                User_Map     => <>,
                                Identity_Map => <>);
   --  Singleton instance.

   -------------------
   --  Assign_Call  --
   -------------------

   procedure Assign_Call (Object  :    out Instance;
                          User_ID : in     Model.User.Identifications;
                          Call_ID : in     Model.Call.Identification) is

      procedure Update (Key     : in     Model.User.Identifications;
                        Element : in out Model.User.Instance);

      procedure Update (Key     : in     Model.User.Identifications;
                        Element : in out Model.User.Instance) is
         pragma Unreferenced (Key);
      begin
         Element.Current_Call := Call_ID;
      end Update;
   begin
      Object.User_Map.Update_Element
        (Object.User_Map.Find (User_ID), Update'Access);
   end Assign_Call;

   ------------------------
   --  Assign_Websocket  --
   ------------------------

   procedure Assign_Websocket
     (Object    :    out Instance;
      User_ID   : in     User.Identifications;
      WebSocket : in     Handlers.Notifications.Object) is

      procedure Update (Key     : in     Model.User.Identifications;
                        Element : in out Model.User.Instance);

      procedure Update (Key     : in     Model.User.Identifications;
                        Element : in out Model.User.Instance) is
         pragma Unreferenced (Key);
      begin
         Element.WebSocket := WebSocket;
      end Update;
   begin
      Object.User_Map.Update_Element
        (Object.User_Map.Find (User_ID), Update'Access);
   end Assign_Websocket;

   -----------
   --  Get  --
   -----------

   function Get (Object   : in Instance;
                 Identity : in User.Identities) return User.Instance is
      use Lookup_Storage;
   begin

      return Object.User_Map.Element (Object.Identity_Map.Element (Identity));
   exception
      when Constraint_Error =>
         System_Messages.Debug (Message => "Identity " &
                                  To_String (Identity) & " not found in map.",
                                Context => "Get");
         return No_User;
   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Object  : in Instance;
                 User_ID : in User.Identifications) return User.Instance is
   begin
      return Object.User_Map.Element (User_ID);
   exception
      when Constraint_Error =>
         return No_User;
   end Get;

   ---------------------
   --  Get_Singleton  --
   ---------------------

   function Get_Singleton return Reference is
   begin
      return Users'Access;
   end Get_Singleton;

   ------------------
   --  Reload_Map  --
   ------------------

   procedure Reload_Map (Object : out Instance) is
      use System_Messages;

      Context : constant String := Package_Name & ".Reload_Map";

      File         : File_Type;
      In_JSON      : JSON_Value;
      User_Arr     : JSON_Array;
      New_Instance : User.List.Instance;
      Filename     : String renames To_String (Object.Path);
   begin
      Open (File => File, Mode => In_File,
            Name => Filename);
      In_JSON := GNATCOLL.JSON.Read (Strm => Get_Line (File));
      User_Arr := In_JSON.Get (User.Users_String);
      for I in 1 .. Length (User_Arr) loop
         declare
            Node          : constant JSON_Value      := Get (User_Arr, I);
            Identity_List : JSON_Array renames
              Node.Get (User.Identities_String);
            ID_String     : String renames Node.Get (User.ID_String);
            ID            : User.Identifications renames
              User.Identifications'Value (ID_String);
         begin
            New_Instance.User_Map.Insert
              (Key      => ID,
               New_Item => Model.User.Create
                 (User_ID => ID,
                  Object  => Node));

            for J in 1 .. Length (Identity_List) loop
               New_Instance.Identity_Map.Insert
                 (Key      => Value (Get (Get (Identity_List, J))),
                  New_Item => ID);
            end loop;

         end;
      end loop;

      Object := New_Instance;

      Information (Message => "Loaded user information from file " & Filename,
                   Context => Context);
   exception
      when E : Constraint_Error | Name_Error =>
         Critical (Message => "Failed to load user information from file "
                   & Filename,
                   Context => Context);
         Critical (Message => "Failed to load user information from file "
                   & Ada.Exceptions.Exception_Information (E),
                   Context => Context);
   end Reload_Map;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON (Object : in Instance) return JSON_Value is
      use User_Storage;
      JSON_List : JSON_Array;
      Root      : constant JSON_Value := Create_Object;
   begin
      for C in Object.User_Map.Iterate loop
         Append (JSON_List, Element (C).To_JSON);
      end loop;
      Root.Set_Field (User.Users_String, JSON_List);
      return Root;
   end To_JSON;
end Model.User.List;
