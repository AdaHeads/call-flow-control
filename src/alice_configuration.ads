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
with Util.Command_Line;
with Util.Config_File_Parser;

package Alice_Configuration is

   use Ada.Strings.Unbounded;
   use Common;

   type Loglevels is (Debug, Information, Error, Warning, Critical, Fixme);

   subtype PBX_Loglevels is Loglevels;

   type Keys is (Cache_Max_Element_Age,
                 Host_Name,
                 Public_User_Identification,
                 DB_Host,
                 DB_Name,
                 DB_Password,
                 DB_Port,
                 DB_User,
                 Loglevel,
                 SQLite_Database,
                 PBX_Secret,
                 PBX_Loglevel,
                 PBX_Host,
                 PBX_Port,
                 User_Backend_Type,
                 User_Map_File,
                 Client_Config_File);

   type Defaults_Array is array (Keys) of Unbounded_String;

   Default_Values : constant Defaults_Array :=
                      (Loglevel
                       => U ("Warning"),
                       Cache_Max_Element_Age
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
                       PBX_Loglevel
                       => U ("Information"),
                       PBX_Host
                       => U ("FreeSWITCH_Host"),
                       PBX_Port
                       => U ("8021"),
                       User_Backend_Type
                       => U ("file"),
                       User_Map_File
                       => U ("static_json/agent.list"),
                       Client_Config_File
                       => U ("configuration/bob_configuration.json"));

   package Config is new Util.Config_File_Parser
     (Key_Type            => Keys,
      Defaults_Array_Type => Defaults_Array,
      Defaults            => Default_Values,
      Config_File         => Util.Command_Line.Get
                               (Parameter => "--alice-config-file",
                                Default   =>
                                "configuration/alice_config.ini"));

   function PBX_Loglevel return PBX_Loglevels;

   function Loglevel return PBX_Loglevels;

private

   Loglevel_CL_String     : constant String := "--loglevel";
   PBX_Loglevel_CL_String : constant String := "--pbx-loglevel";
   PBX_Host_CL_String     : constant String := "--pbx-host";
   PBX_Port_CL_String     : constant String := "--pbx-port";
   PBX_Secret_CL_String   : constant String := "--pbx-secret";

end Alice_Configuration;
