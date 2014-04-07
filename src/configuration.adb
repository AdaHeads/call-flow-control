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

with Ada.Text_IO;
with Configuration.Default;
with Util.Config_File_Parser;
with Util.Command_Line;

package body Configuration is
   use Configuration.Default;

   Current_Loglevel     : Loglevels;
   Current_PBX_Loglevel : PBX_Loglevels;

   package Config is new Util.Config_File_Parser
     (Key_Type            => Configuration.Default.Keys,
      Defaults_Array_Type => Configuration.Default.Defaults_Array,
      Defaults            => Configuration.Default.Default_Values,
      Config_File         => Default_Config_File);

   function Access_Log return String is
   begin
      return Config.Get (Key => Access_Log);
   end Access_Log;

   function Auth_Server return String is
   begin
      return Config.Get (Key => Auth_Server);
   end Auth_Server;

   function Contact_Server return String is
   begin
      return Config.Get (Key => Contact_Server);
   end Contact_Server;

   function Error_Log return String is
   begin
      return Config.Get (Key => Error_Log);
   end Error_Log;

   function HTTP_Port return GNAT.Sockets.Port_Type is
   begin
      return Config.Get (Key => HTTP_Port);
   end HTTP_Port;

   procedure Load_Config is
   begin
      Config.Load_File
        (Config_File => Util.Command_Line.Get
           (Parameter => "--config",
            Default   => Default_Config_File));

      --  Validate config and set command line values.
      Current_Loglevel     := Loglevels'Value
        (Util.Command_Line.Get
           (Parameter => Loglevel_CL_String,
            Default   => Config.Get (Key => Loglevel)));

      Current_PBX_Loglevel := PBX_Loglevels'Value
        (Util.Command_Line.Get
           (Parameter => PBX_Loglevel_CL_String,
            Default   => Config.Get (Key => PBX_Loglevel)));

      Config.Update (Key       => PBX_Host,
                     New_Value =>
                       (Util.Command_Line.Get
                          (Parameter => PBX_Host_CL_String,
                           Default   => Config.Get (Key => PBX_Host))));

      Config.Update (Key       => PBX_Port,
                     New_Value =>
                       (Util.Command_Line.Get
                          (Parameter => PBX_Port_CL_String,
                           Default   => Config.Get (Key => PBX_Port))));

      Config.Update (Key       => PBX_Secret,
                     New_Value =>
                       (Util.Command_Line.Get
                          (Parameter => PBX_Secret_CL_String,
                           Default   => Config.Get (Key => PBX_Secret))));

   end Load_Config;

   ----------------
   --  Loglevel  --
   ----------------

   function Loglevel return PBX_Loglevels is
   begin
      return Current_Loglevel;
   end Loglevel;

   ----------------
   --  PBX_Host  --
   ----------------

   function PBX_Host return String is
   begin
      return Config.Get (Key => PBX_Host);
   end PBX_Host;

   --------------------
   --  PBX_Loglevel  --
   --------------------

   function PBX_Loglevel return PBX_Loglevels is
   begin
      return Current_PBX_Loglevel;
   end PBX_Loglevel;

   --------------------
   --  PBX_Password  --
   --------------------

   function PBX_Password return String is
   begin
      return Config.Get (Key => PBX_Secret);
   end PBX_Password;

   ----------------
   --  PBX_Port  --
   ----------------

   function PBX_Port return Natural is
   begin
      return Config.Get (Key => PBX_Port);
   end PBX_Port;

   ----------------------
   --  Show_Arguments  --
   ----------------------

   procedure Show_Arguments is
      use Ada.Text_IO;

      function Header return String;

      function Header return String is
      begin
         return "Usage:" &
           Util.Command_Line.Command_Name &
           " [--config FILE] [OPTIONS]" & ASCII.LF & ASCII.LF &
           "Command line options available:";
      end Header;

      Indention : constant String := "   ";
      Separator : constant String := ASCII.HT & ASCII.HT;
   begin
      Put_Line (Header);
      Put_Line (Indention & Loglevel_CL_String & Separator &
                  Loglevel_Usage_String);
      Put_Line (Indention & PBX_Loglevel_CL_String & ASCII.HT &
                  PBX_Loglevel_Usage_String);
      Put_Line (Indention & PBX_Host_CL_String & Separator &
                  PBX_Host_Usage_String);
      Put_Line (Indention & PBX_Port_CL_String & Separator &
                  PBX_Port_Usage_String);
      Put_Line (Indention & PBX_Secret_CL_String & Separator &
                  PBX_Secret_Usage_String);
   end Show_Arguments;

end Configuration;
