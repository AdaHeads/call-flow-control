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

with Ada.Strings;
with Ada.Strings.Fixed;

with AMI.Parser;

package body AMI.Packet.Action is
   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;

   use AMI.Parser;

   procedure Add_Field (List  : in out AMI.Packet.Field.Field_List.List;
                        Key   : in     AMI.Parser.AMI_Key_Type;
                        Value : in     String);
   --  Small wrapper function that cuts down on implementation code.

   function Create (Action : in Valid_Action;
                    Fields : in Field_List.List :=
                      Field_List.Empty_List;
                    On_Response : in Response_Handler_Type :=
                      Null_Reponse_Handler'Access) return Request;
   --  Private constructor for intializing an basic object

   ----------------------
   -- Absolute_Timeout --
   ----------------------

   function Absolute_Timeout
     (Channel     : in  String;
      Timeout     : in     Duration := Duration'First;
      On_Response : in     Response_Handler_Type
      := Null_Reponse_Handler'Access
     ) return Request
   is

      Fields : AMI.Packet.Field.Field_List.List :=
       AMI.Packet.Field.Field_List.Empty_List;
      Timeout_Milli_Seconds : constant Natural := Natural (Timeout * 1_000);
   begin
      Add_Field (List  => Fields,
                 Key   => AMI.Parser.Channel,
                 Value => Channel);

      if Timeout > Duration'First then
         Add_Field (List  => Fields,
                    Key   => AMI.Parser.Timeout,
                    Value => Trim (Natural'Image (Timeout_Milli_Seconds),
                      Both));
      end if;

      return Action.Create (Action      => AbsoluteTimeout,
                            Fields      => Fields,
                            On_Response => On_Response);
   end Absolute_Timeout;

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field (R : in out Request;
                        F : in     AMI.Packet.Field.Field) is
   begin
      R.Fields.Append (New_Item => F);
   end Add_Field;

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field (List  : in out AMI.Packet.Field.Field_List.List;
                        Key   : in     AMI.Parser.AMI_Key_Type;
                        Value : in     String) is
   begin
      List.Append (AMI.Packet.Field.Create (Key   => Key,
                                            Value => Value));
   end Add_Field;

   -------------------
   -- Agent_Logoff --
   -------------------

   function Agent_Logoff
     (Agent       : in String;
      Soft        : in Boolean := False;
      On_Response : in     Response_Handler_Type := Null_Reponse_Handler'Access
     ) return Request
   is
      Fields : AMI.Packet.Field.Field_List.List :=
                 AMI.Packet.Field.Field_List.Empty_List;
   begin
      Add_Field (List  => Fields,
                 Key   => AMI.Parser.Agent,
                 Value => Agent);
      if Soft then
         Add_Field (List  => Fields,
                    Key   => AMI.Parser.Soft,
                    Value => Trim (Soft'Img, Left));
      end if;
      return Action.Create (Action      => AgentLogoff,
                            Fields      => Fields,
                            On_Response => On_Response);

   end Agent_Logoff;

   function Agents
     (On_Response : in Response_Handler_Type
      := Null_Reponse_Handler'Access
     ) return Request
   is
   begin
      return Action.Create (Action      => Agents,
                            Fields      => Field_List.Empty_List,
                            On_Response => On_Response);
   end Agents;
   ---------
   -- AGI --
   ---------

   function AGI
     (Channel     : in String;
      Command     : in String;
      CommandID   : in String;
      On_Response : in Response_Handler_Type := Null_Reponse_Handler'Access
     ) return Request
   is
      Fields : AMI.Packet.Field.Field_List.List :=
                 AMI.Packet.Field.Field_List.Empty_List;
   begin
      Add_Field (List  => Fields,
                 Key   => AMI.Parser.Channel,
                 Value => Channel);

      Add_Field (List  => Fields,
                 Key   => AMI.Parser.Command,
                 Value => Command);

      Add_Field (List  => Fields,
                 Key   => AMI.Parser.CommandID,
                 Value => CommandID);

      return Action.Create (Action      => AGI,
                            Fields      => Fields,
                            On_Response => On_Response);
   end AGI;

   ------------
   -- Atxfer --
   ------------

   function Atxfer
     (Channel     : in String;
      Extension   : in String;
      Context     : in String;
      Priority    : in Natural;
      On_Response : in Response_Handler_Type
      := Null_Reponse_Handler'Access
     ) return Request
   is
      Fields : AMI.Packet.Field.Field_List.List :=
                 AMI.Packet.Field.Field_List.Empty_List;
   begin
      Add_Field (List  => Fields,
                 Key   => AMI.Parser.Channel,
                 Value => Channel);

      Add_Field (List  => Fields,
                 Key   => AMI.Parser.Extension,
                 Value => Extension);

      Add_Field (List  => Fields,
                 Key   => AMI.Parser.Context,
                 Value => Context);

      Add_Field (List  => Fields,
                 Key   => AMI.Parser.Priority,
                 Value => Priority'Img);

      return Action.Create (Action      => Atxfer,
                            Fields      => Fields,
                            On_Response => On_Response);
   end Atxfer;
   ------------
   -- Create --
   ------------

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

   ------------
   -- Hangup --
   ------------

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

   ----------
   -- Ping --
   ----------

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
           (Buffer, To_Unbounded_String (String
            (To_AMI_Line
               (Create (ActionID, Trim (R.Action_ID'Img, Both))))));
      end if;

      for Item of R.Fields loop
         Append (Buffer,
                 To_Unbounded_String (String (Item.To_AMI_Line)));
      end loop;

      return AMI_Packet (To_String (Buffer) & Line_Termination_String);
   end To_AMI_Packet;

   ---------------
   -- To_String --
   ---------------

   function To_String (Mask : in Event_Mask) return String is
      Buffer : Unbounded_String := To_Unbounded_String ("");
   begin
      if not Mask.On then
         Append (Buffer, "off");
      else
         for I in Event_Masks'Range loop
            Append (Buffer, Mask.Masks (I)'Img);
            if I /= Event_Masks'Last then
               Append (Buffer, ", ");
            end if;
         end loop;

         --  At this point, if the buffer is still empty, we acknowledge that
         --  this is merely a "on" request.
         if To_String (Buffer) = "" then
            Append (Buffer, "on");
         end if;
      end if;

      pragma Assert (To_String (Buffer) /= "");
      return To_String (Buffer);
   end To_String;

end AMI.Packet.Action;
