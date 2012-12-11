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

-- Deprecated versions of AMI action packets.

package body AMI.Packet.Action.Deprecated is
   --------------------------
   -- Agent_Callback_Login --
   --------------------------

   function Agent_Callback_Login
     (Agent              : in String;
      Extension          : in String;
      Context            : in String := "";
      Acknlowledgde_Call : in Boolean := False;
      WrapupTime         : in Duration := Duration'First;
      On_Response        : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
     ) return Request
   is
      Fields : AMI.Packet.Field.Field_List.List :=
                 AMI.Packet.Field.Field_List.Empty_List;
      WrapupTime_Milli_Seconds : constant Natural :=
       Natural (WrapupTime * 1_000);
   begin
      Add_Field (List  => Fields,
                 Key   => AMI.Parser.Agent,
                 Value => Agent);

      Add_Field (List  => Fields,
                 Key   => AMI.Parser.Extension,
                 Value => Extension);
      if Context /= "" then
         Add_Field (List  => Fields,
                    Key   => AMI.Parser.Context,
                    Value => Context);
      end if;

      if Acknlowledgde_Call then
         Add_Field (List  => Fields,
                    Key   => AMI.Parser.Extension,
                    Value => Trim (WrapupTime_Milli_Seconds'Img, Both));
      end if;

      if WrapupTime > Duration'First then
         Add_Field (List  => Fields,
                    Key   => AMI.Parser.WrapupTime,
                    Value => Trim
                      (Natural'Image (WrapupTime_Milli_Seconds), Both));
      end if;

      return Action.Create (Action      => AgentCallbackLogin,
                            Fields      => Fields,
                            On_Response => On_Response);
   end Agent_Callback_Login;

end AMI.Packet.Action.Deprecated;
