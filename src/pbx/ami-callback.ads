-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              AMI.Callback                                 --
--                                                                           --
--                                  SPEC                                     --
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
with AMI.Client;
with AMI.Packet.Action;

package AMI.Callback is
   use AMI.Parser;
   use AMI.Client;

   type Callback_Type is access procedure (Client : access Client_Type;
                                           Packet : in     Packet_Type);
   --  Signature for callback

   procedure Login_Callback (Client : access Client_Type;
                             Packet : in     Packet_Type);
   --  Default procedure for handling login responses

   procedure Null_Callback (Client : access Client_Type;
                            Packet : in     Packet_Type);

   procedure Ping_Callback (Client : access Client_Type;
                            Packet : in     Packet_Type);

--     procedure ExtensionState (Client : access Client_Type;
--                               Packet : in     Packet_Type);
end AMI.Callback;
