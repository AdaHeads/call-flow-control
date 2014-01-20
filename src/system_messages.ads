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

with Ada.Exceptions;

package System_Messages is

   Package_Name : constant String := "System_Messages";

   procedure Open_Log_Files;
   procedure Close_Log_Files;
   --  Log file handle control.

   procedure Access_Log (Message : in String);

   procedure Debug (Message : in String;
                    Context : in String);

   procedure Information (Message : in String;
                          Context : in String);

   procedure Critical (Message : in String;
                       Context : in String);

   procedure Critical_Exception
     (Message : in String;
      Event   : in Ada.Exceptions.Exception_Occurrence;
      Context : in String);

   procedure Error (Message : in String;
                    Context : in String);

   procedure Error_Log (Message : in String);

   procedure Fixme (Message : in String;
                    Context : in String);

private
   Separator : constant String := ": ";
end System_Messages;
