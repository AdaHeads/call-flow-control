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
with Util.Process_Control;

package body Configuration is

   Current_Loglevel     : Loglevels;
   Current_PBX_Loglevel : PBX_Loglevels;

   ----------------
   --  Loglevel  --
   ----------------

   function Loglevel return PBX_Loglevels is
   begin
      return Current_Loglevel;
   end Loglevel;

   --------------------
   --  PBX_Loglevel  --
   --------------------

   function PBX_Loglevel return PBX_Loglevels is
   begin
      return Current_PBX_Loglevel;
   end PBX_Loglevel;

   --------------------
   --  PBX_Loglevel  --
   --------------------

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

begin
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

exception
   when Constraint_Error =>
      Util.Process_Control.Stop;
      --  At this point, we should perform a graceful shutdown but we need a
      --  mechanism for shutting down the server from here.
end Configuration;
