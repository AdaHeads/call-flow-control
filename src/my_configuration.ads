-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                             My_Configuration                              --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                      --
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
with Yolk.Config_File_Parser;
with Yolk.Utilities;

package My_Configuration is

   package Util renames Yolk.Utilities;

   type Keys is (Cache_Max_Element_Age,
                 Cache_Size_Contact,
                 Cache_Size_Organization,
                 DB_Host,
                 DB_Name,
                 DB_Password,
                 DB_Port,
                 DB_User,
                 DB2_Host,
                 DB2_Name,
                 DB2_Password,
                 DB2_Port,
                 DB2_User,
                 Handler_Call_Hangup,
                 Handler_Call_Pickup,
                 Handler_Call_Hold,
                 Handler_Contact,
                 Handler_Organization,
                 Handler_Queue,
                 Handler_Queue_Length,
                 Handler_Notifications,
                 PBX_Action_Secret,
                 PBX_Action_User,
                 PBX_Event_Secret,
                 PBX_Event_User,
                 PBX_Host,
                 PBX_Port);

   type Defaults_Array is array (Keys) of
     Ada.Strings.Unbounded.Unbounded_String;

   Default_Values : constant Defaults_Array :=
                      (Cache_Max_Element_Age
                       => Util.TUS ("86_400"),
                       Cache_Size_Contact
                       => Util.TUS ("10_000"),
                       Cache_Size_Organization
                       => Util.TUS ("1_000"),
                       DB_Host
                       => Util.TUS ("pg.adaheads.com"),
                       DB_Name
                       => Util.TUS ("customers"),
                       DB_Password
                       => Util.TUS ("secret"),
                       DB_Port
                       => Util.TUS ("5432"),
                       DB_User
                       => Util.TUS ("alice"),
                       DB2_Host
                       => Util.TUS ("pg2.adaheads.com"),
                       DB2_Name
                       => Util.TUS ("customers"),
                       DB2_Password
                       => Util.TUS ("secret"),
                       DB2_Port
                       => Util.TUS ("5432"),
                       DB2_User
                       => Util.TUS ("alice"),
                       Handler_Call_Hangup
                       => Util.TUS ("/call/hangup"),
                       Handler_Call_Hold
                       => Util.TUS ("/call/hold"),
                       Handler_Call_Pickup
                       => Util.TUS ("/call/pickup"),
                       Handler_Contact
                       => Util.TUS ("/contact"),
                       Handler_Organization
                       => Util.TUS ("/organization"),
                       Handler_Queue
                       => Util.TUS ("/queue"),
                       Handler_Queue_Length
                       => Util.TUS ("/queue_length"),
                       Handler_Notifications
                       => Util.TUS ("/notifications"),
                      PBX_Action_Secret
                       => Util.TUS ("reaction"),
                      PBX_Action_User
                       => Util.TUS ("action"),
                      PBX_Event_Secret
                       => Util.TUS ("filtertest"),
                      PBX_Event_User
                       => Util.TUS ("filtertest"),
                      PBX_Host
                       => Util.TUS ("asterisk1"),
                      PBX_Port
                       => Util.TUS ("5038"));

   package Config is new Yolk.Config_File_Parser
     (Key_Type            => Keys,
      Defaults_Array_Type => Defaults_Array,
      Defaults            => Default_Values,
      Config_File         => "configuration/alice_config.ini");

end My_Configuration;
