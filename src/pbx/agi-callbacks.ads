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

with AMI.Parser;

package AGI.Callbacks is

   procedure Event
     (Packet : in AMI.Parser.Packet_Type);
   --  TODO: write comment
   --
   --  Event: AsyncAGI
   --  Privilege: agi,all
   --  SubEvent: Start
   --  Channel: SIP/0000FFFF0001-00000000
   --  Env: agi_request%3A%20async%0A \
   --       agi_channel%3A%20SIP%2F0000FFFF0001-00000000%0A \
   --       agi_language%3A%20en%0A \
   --       agi_type%3A%20SIP%0Aagi_uniqueid%3A%201285219743.0%0A \
   --       agi_version%3A%201.8.0-beta5%0Aagi_callerid%3A%2012565551111%0A \
   --       agi_calleridname%3A%20Julie%20Bryant%0Aagi_callingpres%3A%200%0A \
   --       agi_callingani2%3A%200%0Aagi_callington%3A%200%0A \
   --       agi_callingtns%3A%200%0A \
   --       agi_dnid%3A%20111%0Aagi_rdnis%3A%20unknown%0A \
   --       agi_context%3A%20LocalSets%0A \
   --       agi_extension%3A%20111%0Aagi_priority%3A%201%0A \
   --       agi_enhanced%3A%200.0%0A \
   --       agi_accountcode%3A%20%0Aagi_threadid%3A%20-1339524208%0A%0A

end AGI.Callbacks;
