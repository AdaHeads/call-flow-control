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

with
  Ada.Containers.Vectors;

package body AMI.Observers is
   package Callback_Collections is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => AMI.Event.Event_Callback,
                                 "="          => AMI.Event."=");

   Callbacks : array (AMI.Event.Event_Type) of Callback_Collections.Vector;

   procedure Register (Event   : in     AMI.Event.Event_Type;
                       Handler : in     AMI.Event.Event_Callback) is
   begin
      if not Callbacks (Event).Contains (Handler) then
         Callbacks (Event).Append (Handler);
      end if;
   end Register;

   procedure Notify (Event  : in     AMI.Event.Event_Type;
                     Packet : in     AMI.Parser.Packet_Type) is
   begin
      for Callback of Callbacks (Event) loop
         Callback (Packet);
      end loop;
   end Notify;
end AMI.Observers;
