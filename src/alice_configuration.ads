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
with Util.Config_File_Parser;
with Util.Command_Line;

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
                 Auth_Server,
                 Client_Config_File,
                 Access_Log,
                 Error_Log);

   type Defaults_Array is array (Keys) of Unbounded_String;

   Default_Values : constant Defaults_Array :=
     (Auth_Server
      => U ("http://auth.adaheads.com"),
      Loglevel
      => U ("Warning"),
      Access_Log
      => U ("access.log"),
      Error_Log
      => U ("error.log"),
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

   function PBX_Loglevel return PBX_Loglevels;

   function Loglevel return PBX_Loglevels;

   package Config is new Util.Config_File_Parser
     (Key_Type            => Keys,
      Defaults_Array_Type => Defaults_Array,
      Defaults            => Default_Values,
      Config_File         =>  Util.Command_Line.Get
        (Parameter => "--config",
         Default   =>
         "configuration/alice_config.ini"));

   procedure Show_Arguments;


private

   Loglevel_CL_String     : constant String := "--loglevel";
   Loglevel_Usage_String  : constant String :=
     "Controls the verbosity of the server. Possible values: "&
     "Debug, Information, Error, Warning, Critical.";

   PBX_Loglevel_CL_String : constant String := "--pbx-loglevel";
   PBX_Loglevel_Usage_String  : constant String :=
     "Controls the verbosity of the PBX layer. Possible values: "&
     "Debug, Information, Error, Warning, Critical.";
   PBX_Host_CL_String     : constant String := "--pbx-host";
   PBX_Host_Usage_String  : constant String :=
     "Override the PBX host from config.";
   PBX_Port_CL_String     : constant String := "--pbx-port";
   PBX_Port_Usage_String  : constant String :=
     "Override the PBX port from config.";
   PBX_Secret_CL_String   : constant String := "--pbx-secret";
   PBX_Secret_Usage_String  : constant String :=
     "Override the PBX password from config.";

end Alice_Configuration;
