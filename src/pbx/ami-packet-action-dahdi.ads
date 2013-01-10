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

package AMI.Packet.Action.DAHDI is

   function Dial_Offhook
     (DAHDIChannel : in String;
      --  DAHDI channel number to dial digits.
      Number       : in String;
      --  Digits to dial.
      On_Response  : in Response_Handler_Type := Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  Dial over DAHDI channel while offhook.

   function DND_Off
     (DAHDIChannel : in String;
      --  DAHDI channel number to set DND off.
      On_Response  : in Response_Handler_Type := Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  Toggle DAHDI channel Do Not Disturb status OFF.

   function DND_On
     (DAHDIChannel : in String;
      --  DAHDI channel number to set DND on.
      On_Response  : in Response_Handler_Type := Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  Toggle DAHDI channel Do Not Disturb status ON.

   function Hangup
     (DAHDIChannel : in String;
      --  DAHDI channel number to hang up.
      On_Response  : in Response_Handler_Type := Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  Hangup DAHDI Channel.

   function Restart
     (On_Response : in Response_Handler_Type := Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  Fully Restart DAHDI channels (terminates calls).

   function Show_Channels
     (DAHDIChannel : in String := "";
      --  Specify the specific channel number to show.
      --  Show all channels if not specified.
      On_Response  : in Response_Handler_Type := Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  Show status of DAHDI channels.

   function Transfer
     (DAHDIChannel : in String;
      --  DAHDI channel number to transfer.
      On_Response  : in Response_Handler_Type := Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request;
   --  Transfer DAHDI Channel.

end AMI.Packet.Action.DAHDI;
