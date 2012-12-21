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
with Ada.Characters.Handling;

package body AMI.Packet.Action is
   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;
   use Ada.Characters.Handling;

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
   -- Action_ID --
   ---------------

   function Action_ID (R : in Request) return Action_ID_Type is
   begin
      return R.Action_ID;
   end Action_ID;

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
     (On_Response : in Response_Handler_Type := Null_Reponse_Handler'Access
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
                 Key   => AMI.Parser.Exten,
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
   -- Bridge --
   ------------

   function Bridge
     (Channel1 : in String;
      Channel2 : in String;
      Tone     : in Boolean := False;
      On_Response : in     Response_Handler_Type := Null_Reponse_Handler'Access
     ) return Request
   is
      Fields : AMI.Packet.Field.Field_List.List :=
                 AMI.Packet.Field.Field_List.Empty_List;
   begin
      Add_Field (List  => Fields,
                 Key   => AMI.Parser.Channel1,
                 Value => Channel1);

      Add_Field (List  => Fields,
                 Key   => AMI.Parser.Channel2,
                 Value => Channel2);

      if Tone then
         Add_Field (List  => Fields,
                    Key   => AMI.Parser.Tone,
                    Value => "yes");
      else
         Add_Field (List  => Fields,
                    Key   => AMI.Parser.Tone,
                    Value => "no");
      end if;

      return Action.Create (Action      => Valid_Action'(Bridge),
                            Fields      => Fields,
                            On_Response => On_Response);
   end Bridge;

   ---------------
   -- Challenge --
   ---------------

   function Challenge
     (Authentication_Type : in Valid_Authentications := MD5;
      On_Response         : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
     ) return Request
   is
      Fields : AMI.Packet.Field.Field_List.List :=
                 AMI.Packet.Field.Field_List.Empty_List;
   begin
      Add_Field (List  => Fields,
                 Key   => AMI.Parser.AuthType,
                 Value => To_Lower (Authentication_Type'Img));

      return Action.Create (Action      => Valid_Action'(Challenge),
                            Fields      => Fields,
                            On_Response => On_Response);
   end Challenge;

   --------------------
   -- Change_Monitor --
   --------------------

   function Change_Monitor
     (Channel     : in String;
      File        : in String;
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
                 Key   => AMI.Parser.File,
                 Value => File);

      return Action.Create (Action      => Valid_Action'(ChangeMonitor),
                            Fields      => Fields,
                            On_Response => On_Response);
   end Change_Monitor;

   -------------
   -- Command --
   -------------

   function Command
     (In_Command  : in String;
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
     ) return Request
   is
      Fields : AMI.Packet.Field.Field_List.List :=
                 AMI.Packet.Field.Field_List.Empty_List;
   begin
      Add_Field (List  => Fields,
                 Key   => AMI.Parser.Command,
                 Value => In_Command);

      return Action.Create (Action      => Valid_Action'(Command),
                            Fields      => Fields,
                            On_Response => On_Response);
   end Command;

   -------------------
   -- Core_Settings --
   -------------------

   function Core_Settings
     (On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
     ) return Request
   is
   begin

      return Action.Create (Action      => Valid_Action'(CoreSettings),
                            Fields      => Field.Field_List.Empty_List,
                            On_Response => On_Response);
   end Core_Settings;

   ------------------------
   -- Core_Show_Channels --
   ------------------------

   function Core_Show_Channels
     (On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
     ) return Request
   is
   begin
      return Action.Create (Action      => Valid_Action'(CoreShowChannels),
                            Fields      => Field.Field_List.Empty_List,
                            On_Response => On_Response);
   end Core_Show_Channels;

   -----------------
   -- Core_Status --
   -----------------

   function Core_Status
     (On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
     ) return Request is
   begin
      return Action.Create (Action      => Valid_Action'(CoreStatus),
                            Fields      => Field.Field_List.Empty_List,
                            On_Response => On_Response);
   end Core_Status;

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

   ---------------------
   --  Create_Config  --
   ---------------------

   function Create_Config
     (Filename    : in String;
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
      --  The reponse handler
     ) return Request
   is
      Fields : AMI.Packet.Field.Field_List.List :=
                 AMI.Packet.Field.Field_List.Empty_List;
   begin
      Fields.Append (AMI.Packet.Field.Create (Key   => AMI.Parser.Filename,
                                              Value => Filename));

      return Action.Create (Action      => CreateConfig,
                            Fields      => Fields,
                            On_Response => On_Response);
   end Create_Config;
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

   -----------
   -- Login --
   -----------

   function Login
     (Username    : in String;
      Secret      : in String;
      On_Response : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
     ) return Request
   is
      Fields : AMI.Packet.Field.Field_List.List :=
                 AMI.Packet.Field.Field_List.Empty_List;
   begin
      Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.Username,
                      Value => Username));

      Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.Secret,
                      Value => Secret));

      return Action.Create (Action      => Login,
                            Fields      => Fields,
                            On_Response => On_Response);
   end Login;

   -----------
   -- Login --
   -----------

   function Logoff
     (On_Response : in Response_Handler_Type := Null_Reponse_Handler'Access
     ) return Request is
   begin
      return Action.Create (Action      => Login,
                            Fields      => Field.Field_List.Empty_List,
                            On_Response => On_Response);
   end Logoff;

   function Originate
     (Channel      : in String;
      Extension    : in String;
      Context      : in String;
      Priority     : in Natural;
      Timeout      : in Duration := 30.0;
      CallerID     : in String := "";
      Variable     : in String := "";
      Account      : in String := "";
      Codecs       : in String := "";
      On_Response  : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
     ) return Request
   is
      Timeout_Milli_Seconds : constant Natural := Natural (Timeout * 1_000);
      Fields                : AMI.Packet.Field.Field_List.List :=
                                AMI.Packet.Field.Field_List.Empty_List;
   begin
      --  Required fields.
      Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.Channel,
                      Value => Channel));

      Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.Exten,
                      Value => Extension));

      Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.Context,
                      Value => Context));

      Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.Priority,
                      Value => Trim (Priority'Img, Left)));

      Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.Timeout,
                      Value => Trim
                        (Natural'Image (Timeout_Milli_Seconds), Left)));

      if CallerID /= "" then
         Fields.Append (AMI.Packet.Field.Create
                        (Key   => AMI.Parser.CallerID,
                         Value => CallerID));
      end if;

      if Variable /= "" then
         Fields.Append (AMI.Packet.Field.Create
                        (Key   => AMI.Parser.Variable,
                         Value => Variable));
      end if;

      if Account /= "" then
         Fields.Append (AMI.Packet.Field.Create
                        (Key   => AMI.Parser.Account,
                         Value => Account));
      end if;

      if Codecs /= "" then
         Fields.Append (AMI.Packet.Field.Create
                        (Key   => AMI.Parser.Codecs,
                         Value => Codecs));
      end if;

      return Action.Create (Action      => Originate,
                            Fields      => Fields,
                            On_Response => On_Response);
   end Originate;

   ----------
   -- Park --
   ----------

   function Park
     (Channel      : in String;
      Channel2     : in String;
      Timeout      : in Duration := 45.0;
      Parkinglot   : String := "";
      On_Response  : in Response_Handler_Type := Null_Reponse_Handler'Access
     ) return Request
   is
      Timeout_Milli_Seconds : constant Natural := Natural (Timeout * 1_000);
      Fields                : AMI.Packet.Field.Field_List.List :=
                                AMI.Packet.Field.Field_List.Empty_List;
   begin
      Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.Channel,
                      Value => Channel));

      Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.Channel2,
                      Value => Channel2));

      Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.Timeout,
                      Value => Trim
                        (Natural'Image (Timeout_Milli_Seconds), Left)));

      if Parkinglot /= "" then
         Fields.Append (AMI.Packet.Field.Create
                        (Key   => AMI.Parser.ParkingLot,
                         Value => Parkinglot));
      end if;

      return Action.Create (Action      => Park,
                            Fields      => Fields,
                            On_Response => On_Response);
   end Park;

   ----------
   -- Ping --
   ----------

   function Ping (On_Response : in Response_Handler_Type :=
                    Null_Reponse_Handler'Access) return Request is
   begin
      return Create (Action => Ping, On_Response => On_Response);
   end Ping;

   --------------
   -- Redirect --
   --------------

   function Redirect
     (Channel         : in String;
      Extra_Channel   : in String := "";
      Extension       : in String;
      Extra_Extension : in String := "";
      Context         : in String;
      Extra_Context   : in String := "";
      Priority        : in Natural := 1;
      Extra_Priority  : in Natural := Natural'Last;
      On_Response     : in Response_Handler_Type :=
        Null_Reponse_Handler'Access
     ) return Request
   is
      Fields                : AMI.Packet.Field.Field_List.List :=
                                AMI.Packet.Field.Field_List.Empty_List;
   begin

      --  Required fields.
      Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.Channel,
                      Value => Channel));

      Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.Exten,
                      Value => Extension));

      Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.Context,
                      Value => Context));

      Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.Priority,
                      Value => Trim (Priority'Img, Left)));

      --  Optional fields.
      if Extra_Channel /= "" then
         Fields.Append (AMI.Packet.Field.Create
                        (Key   => AMI.Parser.ExtraChannel,
                         Value => Extra_Channel));
      end if;

      if Extra_Extension /= "" then
         Fields.Append (AMI.Packet.Field.Create
                        (Key   => AMI.Parser.ExtraExten,
                      Value => Extra_Extension));
      end if;

      if Extra_Context /= "" then
         Fields.Append (AMI.Packet.Field.Create
                        (Key   => AMI.Parser.ExtraContext,
                         Value => Extra_Context));
      end if;

      if Extra_Priority /= Natural'Last then
         Fields.Append (AMI.Packet.Field.Create
                     (Key   => AMI.Parser.ExtraPriority,
                      Value => Trim (Extra_Priority'Img, Left)));
      end if;

      return Action.Create (Action      => Redirect,
                            Fields      => Fields,
                            On_Response => On_Response);
   end Redirect;

   ----------------------
   -- Response_Handler --
   ----------------------

   function Response_Handler (R : in Request) return Response_Handler_Type is
   begin
      return R.Response_Handler;
   end Response_Handler;

   ---------------
   -- SIP_Peers --
   ---------------

   function SIP_Peers
     (On_Response  : in Response_Handler_Type := Null_Reponse_Handler'Access
     ) return Request is
   begin
      return Create (Action      => SIPPeers,
                     Fields      => Field.Field_List.Empty_List,
                     On_Response => On_Response);
   end SIP_Peers;

   -------------------
   -- To_AMI_Packet --
   -------------------

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

   --------------------------
   -- Voicemail_Users_List --
   --------------------------

   function Voicemail_Users_List
     (On_Response  : in Response_Handler_Type := Null_Reponse_Handler'Access)
      return Request
   is
   begin
      return Create (Action      => VoicemailUsersList,
                     Fields      => Field.Field_List.Empty_List,
                     On_Response => On_Response);
   end Voicemail_Users_List;

   ----------------
   -- Wait_Event --
   ----------------

   function Wait_Event
     (Timeout      : in Duration := 30.0;
      On_Response  : in Response_Handler_Type := Null_Reponse_Handler'Access
     ) return Request
   is
      Fields                : AMI.Packet.Field.Field_List.List :=
                                AMI.Packet.Field.Field_List.Empty_List;
      Timeout_Milli_Seconds : constant Natural := Natural (Timeout * 1_000);
   begin
      Add_Field (List  => Fields,
                 Key   => AMI.Parser.Timeout,
                 Value => Trim (Natural'Image (Timeout_Milli_Seconds),
                   Both));

      return Action.Create (Action      => WaitEvent,
                            Fields      => Fields,
                            On_Response => On_Response);
   end Wait_Event;

end AMI.Packet.Action;
