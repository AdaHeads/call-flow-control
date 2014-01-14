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

package body Alice_Configuration is

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

   --  exception
   --     when Constraint_Error =>
   --  At this point, we should perform a graceful shutdown but we need a
   --  mechanism for shutting down the server from here.

end Alice_Configuration;
