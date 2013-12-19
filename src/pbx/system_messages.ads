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

package System_Messages is

   type Message_Type is (Debug, Information, Error, Warning, Critical, Fixme);

   procedure Information (Message : in String;
                          Context : in String);

   procedure Critical (Message : in String;
                       Context : in String);

   procedure Fixme (Message : in String;
                    Context : in String);

   procedure Notify (Level : in Message_Type; Message : in String);

private
   Separator : constant String := ": ";
end System_Messages;
