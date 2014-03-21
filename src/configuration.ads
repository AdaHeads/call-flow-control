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

package Configuration is

   Default_Config_File : constant String := "conf/main.conf";

   type Loglevels is (Debug, Information, Error, Warning, Critical, Fixme);

   subtype PBX_Loglevels is Loglevels;

   function Auth_Server return String;

   function PBX_Host return String;
   function PBX_Port return Natural;
   function PBX_Password return String;

   function Contact_Server return String;

   function Loglevel return Loglevels;

   function Access_Log return String;

   function Error_Log return String;

   function PBX_Loglevel return PBX_Loglevels;

   procedure Show_Arguments;

   procedure Load_Config;

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

end Configuration;
