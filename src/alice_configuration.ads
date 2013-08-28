-------------------------------------------------------------------------------
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

with Ada.Strings.Unbounded;
with Common;
with Yolk.Command_Line;
with Yolk.Config_File_Parser;

package Alice_Configuration is

   use Ada.Strings.Unbounded;
   use Common;

   type Keys is (Cache_Max_Element_Age,
                 Host_Name,
                 Public_User_Identification,
                 DB_Host,
                 DB_Name,
                 DB_Password,
                 DB_Port,
                 DB_User,
                 SQLite_Database,
                 PBX_Secret,
                 PBX_Host,
                 PBX_Port);

   type Defaults_Array is array (Keys) of Unbounded_String;

   Default_Values : constant Defaults_Array :=
                      (Cache_Max_Element_Age
                       => U ("86_400"),
                       Host_Name
                       => U ("alice.adaheads.com"),
                       Public_User_Identification
                       => U ("FALSE"),
                       DB_Host
                       => U ("pgprimary.adaheads.com"),
                       DB_Name
                       => U ("customers"),
                       DB_Password
                       => U ("secret"),
                       DB_Port
                       => U ("5432"),
                       DB_User
                       => U ("alice"),
                       SQLite_Database
                       => U ("sqlite/customers.db"),
                       PBX_Secret
                       => U ("password"),
                       PBX_Host
                       => U ("FreeSWITCH_Host"),
                       PBX_Port
                       => U ("8021"));

   package Config is new Yolk.Config_File_Parser
     (Key_Type            => Keys,
      Defaults_Array_Type => Defaults_Array,
      Defaults            => Default_Values,
      Config_File         => Yolk.Command_Line.Get
                               (Parameter => "--alice-config-file",
                                Default   =>
                                "configuration/alice_config.ini"));

end Alice_Configuration;
