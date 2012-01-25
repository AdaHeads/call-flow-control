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

with Ada.Task_Attributes;
with GNATCOLL.JSON;
with Yolk.Utilities;

package body LDAP is

   package Task_Association is new Ada.Task_Attributes
     (LDAP.Server_Store, LDAP.Null_Store);

   ---------------------
   --  Error_Handler  --
   ---------------------

   function Error_Handler
     (Event       : in Ada.Exceptions.Exception_Occurrence;
      LDAP_Values : in String)
      return String
   is
      use Ada.Exceptions;
      use GNATCOLL.JSON;

      JSON_Object : constant JSON_Value := Create_Object;
   begin
      JSON_Object.Set_Field (Field_Name => "error",
                             Field      => Exception_Message (Event));
      JSON_Object.Set_Field (Field_Name => "parameters",
                             Field      => LDAP_Values);
      return Write (JSON_Object);
   end Error_Handler;

   ---------------------
   --  Get_Directory  --
   ---------------------

   function Get_Directory
     (A_Server : in Server)
     return AWS.LDAP.Client.Directory
   is
   begin
      return A_Server.LDAP_Dir;
   end Get_Directory;

   -------------------------------
   --  Initialize_Server_Store  --
   -------------------------------

   procedure Initialize_Server_Store
     (Store : out Server_Store)
   is
      use AWS.LDAP.Client;
      use Yolk.Utilities;
   begin
      Store.Live_List.Append ((LDAP_Dir   => Null_Directory,
                               Host        => TUS ("alpha.adaheads.com"),
                               Password    => TUS ("D3nSort3H3st"),
                               Port        => 1389,
                               User_DN     => TUS ("cn=Directory Manager"),
                               Death_Stamp => Clock));

      Store.Live_List.Append ((LDAP_Dir   => Null_Directory,
                               Host        => TUS ("beta.adaheads.com"),
                               Password    => TUS ("D3nSort3H3st"),
                               Port        => 1390,
                               User_DN     => TUS ("cn=Directory Manager"),
                               Death_Stamp => Clock));

      Store.Live_List.Append ((LDAP_Dir   => Null_Directory,
                               Host        => TUS ("gamma.adaheads.com"),
                               Password    => TUS ("D3nSort3H3st"),
                               Port        => 1391,
                               User_DN     => TUS ("cn=Directory Manager"),
                               Death_Stamp => Clock));

      Store.Initialized := True;
   end Initialize_Server_Store;

   ------------------
   --  Pop_Server  --
   ------------------

   function Pop_Server
     return Server
   is
      use AWS.LDAP.Client;
      use Yolk.Utilities;

      Result : Server;
      Store  : Server_Store;
   begin
      Store := Task_Association.Value;

      if not Store.Initialized then
         Initialize_Server_Store (Store);
      end if;

      Result := Store.Live_List.First_Element;

      if not Is_Open (Result.LDAP_Dir) then
         Result.LDAP_Dir := Init (TS (Result.Host), Result.Port);
         Bind (Result.LDAP_Dir, TS (Result.User_DN), TS (Result.Password));
      end if;

      Store.Live_List.Delete_First;

      Task_Association.Set_Value (Store);

      return Result;
   end Pop_Server;

   -------------------
   --  Push_Server  --
   -------------------

   procedure Push_Server
     (A_Server : in out Server;
      Is_Alive : in     Boolean := True)
   is
      use AWS.LDAP.Client;

      Store  : Server_Store;
   begin
      Store := Task_Association.Value;

      if Is_Alive then
         Store.Live_List.Append (A_Server);
      else
         A_Server.LDAP_Dir := Null_Directory;
         A_Server.Death_Stamp := Clock;
         Store.Dead_List.Append (A_Server);
      end if;

      Task_Association.Set_Value (Store);
   end Push_Server;

end LDAP;
