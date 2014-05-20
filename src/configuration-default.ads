-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
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

with Ada.Strings.Unbounded;
with Common;

private package Configuration.Default is
   use Ada.Strings.Unbounded;
   use Common;

   type Keys is (Cache_Max_Element_Age,
                 Host_Name,
                 Loglevel,
                 PBX_Secret,
                 PBX_Loglevel,
                 PBX_Host,
                 PBX_Port,
                 Auth_Server,
                 Contact_Server,
                 Notification_Broadcast_URL,
                 Access_Log,
                 Error_Log,
                 Server_Token,
                 Socket_Port,
                 HTTP_Port);

   type Defaults_Array is array (Keys) of Unbounded_String;

   Default_Values : constant Defaults_Array :=
     (Auth_Server                => U ("http://localhost:8080"),
      Contact_Server             => U ("http://localhost:4010"),
      Notification_Broadcast_URL => U ("http://localhost:4200/notifications"),
      Loglevel                   => U ("Information"),
      Access_Log                 => U ("access.log"),
      Error_Log                  => U ("error.log"),
      Cache_Max_Element_Age      => U ("86_400"),
      Host_Name                  => U ("please_update_main_conf"),
      PBX_Secret                 => U ("1234"),
      PBX_Loglevel               => U ("Information"),
      PBX_Host                   => U ("localhost"),
      PBX_Port                   => U ("8021"),
      Server_Token               => U (""),
      HTTP_Port                  => U ("4242"),
      Socket_Port                => U ("9999"));

end Configuration.Default;
