-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                  LDAP                                     --
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

with Ada.Directories;
with Ada.Direct_IO;
with Ada.Task_Attributes;
with Ada.Task_Identification;
with AWS.Utils;
with GNATCOLL.JSON;
with My_Configuration;
with Yolk.Log;
with Yolk.Utilities;

package body LDAP is

   package My renames My_Configuration;

   package Task_Association is new Ada.Task_Attributes
     (LDAP.Server_Store, LDAP.Null_Store);

   procedure Initialize
     (LDAP_JSON : in String);
   --  Setup a chain of Server objects based on the contents of the LDAP_JSON
   --  file. These are then later copied to individual stores of servers for
   --  each running AWS task.

   ---------------------
   --  Error_Handler  --
   ---------------------

   function Error_Handler
     (Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String)
      return String
   is
      use Ada.Exceptions;
      use Ada.Task_Identification;
      use GNATCOLL.JSON;
      use Yolk.Log;

      E_Name : constant String := Exception_Name (Event);
      E_Msg  : constant String := Exception_Message (Event);
      JSON_Object : constant JSON_Value := Create_Object;
   begin
      Trace (Error,
             "Task ID " & Image (Current_Task) &
             " - " & E_Name & " - " & E_Msg & " - " & Message);

      JSON_Object.Set_Field (Field_Name => "exception",
                             Field      => E_Name);
      JSON_Object.Set_Field (Field_Name => "exception_message",
                             Field      => E_Msg);
      JSON_Object.Set_Field (Field_Name => "message",
                             Field      => Message);
      return Write (JSON_Object);
   end Error_Handler;

   ---------------------
   --  Error_Handler  --
   ---------------------

   procedure Error_Handler
     (Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String)
   is
      use Ada.Exceptions;
      use Ada.Task_Identification;
      use Yolk.Log;

      E_Name : constant String := Exception_Name (Event);
      E_Msg  : constant String := Exception_Message (Event);
   begin
      Trace (Error,
             "Task ID " & Image (Current_Task) &
             " - " & E_Name & " - " & E_Msg & " - " & Message);
   end Error_Handler;

   -------------------
   --  Get_Base_DN  --
   -------------------

   function Get_Base_DN
     (A_Server : in Server)
      return String
   is
      use Yolk.Utilities;
   begin
      return TS (A_Server.Base_DN);
   end Get_Base_DN;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize
     (LDAP_JSON : in String)
   is
      use Ada.Directories;
      use GNATCOLL.JSON;

      File_Size : constant Natural := Natural (Size (LDAP_JSON));

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File        : File_String_IO.File_Type;
      LDAP_String : File_String;
      J           : JSON_Value;
      J_Arr       : JSON_Array;
      A_Server    : Server;
   begin
      File_String_IO.Open  (File => File,
                            Mode => File_String_IO.In_File,
                            Name => LDAP_JSON);
      File_String_IO.Read  (File => File,
                            Item => LDAP_String);
      File_String_IO.Close (File);

      J := Read (LDAP_String, "json.debug");
      J_Arr := J.Get (Field => "servers");

      for k in 1 .. Length (J_Arr) loop
         J := Get (J_Arr, k);
         A_Server.LDAP_Dir := AWS.LDAP.Client.Null_Directory;
         A_Server.Host := J.Get ("host");
         A_Server.Port := J.Get ("port");
         A_Server.Password := J.Get ("password");
         A_Server.Base_DN := J.Get ("base_dn");
         A_Server.User_DN := J.Get ("user_dn");
         A_Server.Death_Stamp := Clock;
         A_Server.Coffin_Time := Duration (Float'(J.Get ("coffin_time")));

         Servers_From_JSON.Append (A_Server);
      end loop;
   end Initialize;

   -------------------------------
   --  Initialize_Server_Store  --
   -------------------------------

   procedure Initialize_Server_Store
     (Store : out Server_Store)
   is
      use AWS.LDAP.Client;
      use Yolk.Utilities;
   begin
      Store.Live_List := Servers_From_JSON;
      Store.Initialized := True;
   end Initialize_Server_Store;

   ------------------
   --  Put_Server  --
   ------------------

   procedure Put_Server
     (A_Server : in out Server;
      Is_Alive : in     Boolean := True)
   is
      use Ada.Task_Identification;
      use AWS.LDAP.Client;
      use Yolk.Log;
      use Yolk.Utilities;

      Store  : Server_Store;
   begin
      Store := Task_Association.Value;

      if Is_Alive then
         Store.Live_List.Append (A_Server);
      else
         A_Server.LDAP_Dir := Null_Directory;
         A_Server.Death_Stamp := Clock;
         Store.Dead_List.Append (A_Server);

         Trace (Info,
                "(Put_Server) Task ID " & Image (Current_Task) &
                " Host " & TS (A_Server.Host) & " returned dead.");
         Trace (Info,
                "(Put_Server) Task ID " & Image (Current_Task) &
                " Live list population: " &
                AWS.Utils.Image (Natural (Store.Live_List.Length)));
         Trace (Info,
                "(Put_Server) Task ID " & Image (Current_Task) &
                " Dead list population: " &
                AWS.Utils.Image (Natural (Store.Dead_List.Length)));
      end if;

      Task_Association.Set_Value (Store);
   end Put_Server;

   -------------------
   --  Take_Server  --
   -------------------

   function Take_Server
     return Server
   is
      use Ada.Task_Identification;
      use AWS.LDAP.Client;
      use Yolk.Log;
      use Yolk.Utilities;

      C      : Cursor;
      Now    : constant Time := Clock;
      Result : Server;
      Store  : Server_Store;
   begin
      Store := Task_Association.Value;

      if not Store.Initialized then
         Initialize_Server_Store (Store);
      end if;

      C := Store.Dead_List.First;
      while Has_Element (C) loop
         Result := Element (C);

         if Now - Result.Death_Stamp > Result.Coffin_Time then
            begin
               Result.LDAP_Dir := Init (TS (Result.Host), Result.Port);
               Bind (Result.LDAP_Dir,
                     TS (Result.User_DN),
                     TS (Result.Password));

               Store.Live_List.Append (Result);
               Store.Dead_List.Delete (C);

               --  At this point C equals No_Element, meaning that the call to
               --  Next (C) does absolutely nothing, which in turns results in
               --  the loop exiting.
               --  This is good if there's a huge amount of servers in the dead
               --  list, but it's a bit "meh" if there's just a few. Maybe.
               --
               --  I can't really decide what to do with this.....

               Trace (Info,
                      "(Take_Server) Task ID " & Image (Current_Task) &
                      " Tried reviving " & TS (Result.Host) &
                      " and succeeded. Moving it above ground.");

            exception
               when E : LDAP_Error =>
                  Result.LDAP_Dir := Null_Directory;
                  Result.Death_Stamp := Now;
                  Store.Dead_List.Replace_Element (C, Result);

                  Error_Handler
                    (E,
                     "(Take_Server) Tried reviving " & TS (Result.Host) &
                     " but did not succeed. Leaving it in the coffin" &
                     " and updating Death_Stamp");
            end;
         end if;

         Next (C);
      end loop;

      if not Store.Live_List.Is_Empty then
         Result := Store.Live_List.First_Element;
         Store.Live_List.Delete_First;

         if not Is_Open (Result.LDAP_Dir) then
            Result.LDAP_Dir := Init (TS (Result.Host), Result.Port);
            Bind (Result.LDAP_Dir, TS (Result.User_DN), TS (Result.Password));
         end if;
      else
         raise No_LDAP_Server_Available with "Live list is empty";
      end if;

      Task_Association.Set_Value (Store);

      return Result;

   exception
      when E : LDAP_Error =>
         Error_Handler
           (E,
            "(Take_Server) Cannot bind to " &  TS (Result.Host) &
            ":" & AWS.Utils.Image (Result.Port) &
            " with user_dn " & TS (Result.User_DN) &
            " and password " & TS (Result.Password) &
            " Moving " & TS (Result.Host) & " to dead list");

         Result.Death_Stamp := Clock;
         Store.Dead_List.Append (Result);

         Trace (Info,
                "(Take_Server)  Task ID " & Image (Current_Task) &
                " Live list population: " &
                AWS.Utils.Image (Natural (Store.Live_List.Length)));
         Trace (Info,
                "(Take_Server)  Task ID " & Image (Current_Task) &
                " Dead list population: " &
                AWS.Utils.Image (Natural (Store.Dead_List.Length)));

         Task_Association.Set_Value (Store);

         if not Store.Live_List.Is_Empty then
            return Take_Server;
         else
            raise No_LDAP_Server_Available with "Live list is empty";
         end if;
      when No_LDAP_Server_Available =>
         Task_Association.Set_Value (Store);
         raise;
   end Take_Server;

begin

   Initialize (LDAP_JSON => My.Config.Get (My.LDAP_JSON));

end LDAP;
