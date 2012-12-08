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

with Ada.Strings.Unbounded;
with AMI.Parser;

package body AMI.Packet.Action is
   use Ada.Strings.Unbounded;
   use AMI.Parser;

   function Create (Action : in Valid_Action;
                    Fields : in Field_List.List :=
                      Field_List.Empty_List;
                    On_Response : in Response_Handler_Type :=
                      Null_Reponse_Handler'Access) return Request;
   --  Private constructor for intializing an basic object

   procedure Add_Field (R : in out Request;
                        F : in     AMI.Packet.Field.Field) is
   begin
      R.Fields.Append (New_Item => F);
   end Add_Field;

   function Create (Action : in Valid_Action;
                    Fields : in Field_List.List :=
                      Field_List.Empty_List;
                    On_Response : in Response_Handler_Type :=
                      Null_Reponse_Handler'Access) return Request is
   begin
      return (Asynchronous => True,
              Action       => Action,
              Action_ID    => Next,
              Fields       => Fields,
              Response_Handler => On_Response);
   end Create;

   function Hangup (Channel     : in String;
                    On_Response : in Response_Handler_Type :=
                      Null_Reponse_Handler'Access) return Request is
      Fields : AMI.Packet.Field.Field_List.List :=
                 AMI.Packet.Field.Field_List.Empty_List;
   begin
      Fields.Append (AMI.Packet.Field.Create (Key   => AMI.Parser.Channel,
                                              Value => Channel));

      return Action.Create (Action      => Hangup,
                            Fields      => Fields,
                            On_Response => On_Response);
   end Hangup;

   function Ping (On_Response : in Response_Handler_Type :=
                    Null_Reponse_Handler'Access) return Request is
   begin
      return Create (Action => Ping, On_Response => On_Response);
   end Ping;

   function To_AMI_Packet
     (R : in Request) return AMI_Packet is

      Buffer : Unbounded_String :=
                 To_Unbounded_String
                   ("Action" & Separator & R.Action'Img &
                               Line_Termination_String);
   begin
      --  Append the asynchonous line
      if R.Asynchronous then
         Append
           (Buffer, To_Unbounded_String (String (
            To_AMI_Line (Create (Async, R.Asynchronous'Img)))));
      end if;

      --  Append the Action_ID
      if R.Action_ID /= Null_Action_ID then
         Append
           (Buffer, To_Unbounded_String (String (
               To_AMI_Line (Create (ActionID, R.Action_ID'Img)))));
      end if;

      for Item of R.Fields loop
         Append (Buffer,
                 To_Unbounded_String (String (Item.To_AMI_Line)));
      end loop;

      return AMI_Packet (To_String (Buffer) & Line_Termination_String);
   end To_AMI_Packet;

end AMI.Packet.Action;
