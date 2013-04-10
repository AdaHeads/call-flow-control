-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2013-, AdaHeads K/S                     --
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

package System_Message.Debug is

   procedure Entered_Subprogram is new Logger
     (Log_Trace => Yolk.Log.Debug,
      Status    => "Entered subprogram");

   procedure Leaving_Subprogram is new Logger
     (Log_Trace => Yolk.Log.Debug,
      Status    => "Leaving subprogram");

   procedure Looking_For_XML_Element is new Logger
     (Log_Trace => Yolk.Log.Debug,
      Status    => "Looking for XML element");

   procedure Looking_For_XML_Attribute is new Logger
     (Log_Trace => Yolk.Log.Debug,
      Status    => "Looking for XML attribute");

   procedure Dial_Plan is new Logger
     (Log_Trace => Yolk.Log.Debug,
      Status    => "libdialplan");

end System_Message.Debug;
