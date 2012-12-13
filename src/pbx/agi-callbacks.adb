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
  Ada.Characters.Latin_1,
  Ada.Strings.Fixed,
  Ada.Strings.Unbounded;
with
  AWS.URL;
with
  AMI.Event,
  AMI.Observers,
  AMI.Packet.Action,
  PBX,
  System_Messages;

package body AGI.Callbacks is

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
   procedure Event (Packet : in AMI.Parser.Packet_Type) is
      use System_Messages;

      function Event return String is
      begin
         return Ada.Strings.Unbounded.To_String (Packet.Header.Value);
      end Event;

      function Value (Key : in     AMI.Parser.AMI_Key_Type) return String is
         use Ada.Strings.Unbounded;
      begin
         return AWS.URL.Decode (To_String (Packet.Fields.Element (Key)));
      exception
         when Constraint_Error =>
            System_Messages.Notify
              (Level   => Error,
               Message => "AGI: No " & AMI.Parser.AMI_Key_Type'Image (Key) &
                          " value in packet.");
            return "";
      end Value;

      type AGI_Events is (Start, Exec, Close, Unrecognised);

      function AGI_Event return AGI_Events is
         Descriptor : constant String := Value (AMI.Parser.SubEvent);
      begin
         if Descriptor = "End" then
            return Close;
         else
            return AGI_Events'Value (Descriptor);
         end if;
      exception
         when Constraint_Error =>
            System_Messages.Notify
              (Level   => Error,
               Message => "AGI: Received an '" & Descriptor & "' subevent.  " &
                          "Tell Jacob to add it to type AGI_Events.");
            return Unrecognised;
      end AGI_Event;

      function Channel return String is
      begin
         return Value (AMI.Parser.Channel);
      end Channel;

      function Command_ID return String is
      begin
         return Value (AMI.Parser.CommandID);
      end Command_ID;

      function Result return String is
         use Ada.Strings.Fixed;
         Raw                  : constant String := Value (AMI.Parser.Result);
         First, Second, Third : Positive;
      begin
         First  := Index (Source  => Raw,
                          Pattern => " ");
         Second := Index (Source  => Raw (First  + 1 .. Raw'Last),
                          Pattern => "=");
         Third  := Index (Source  => Raw (Second + 1 .. Raw'Last),
                          Pattern => (1 => Ada.Characters.Latin_1.LF));

         return Raw (Second + 1 .. Third - 1) & " (" &
                Raw (Raw'First .. First - 1) & ")";
      exception
         when others =>
            System_Messages.Notify
              (Level   => Error,
               Message => "AGI: Result field could not be parsed: """ & Raw &
                          """.  Give Jacob a copy of the raw result string.");
            return Raw;
      end Result;

      generic
         Field : String;
      function From_Environment return String;

      function From_Environment return String is
         use Ada.Strings.Fixed;
         Key         : constant String := "agi_" & Field & ": ";
         Terminator  : constant String := (1 => Ada.Characters.Latin_1.LF);
         Environment : constant String := Value (AMI.Parser.Env);
         Key_Position        : Positive;
         Terminator_Position : Positive;
      begin
         Key_Position :=
           Index (Source  => Environment,
                  Pattern => Key);
         Terminator_Position :=
           Index (Source  => Environment
                               (Key_Position + Key'Length .. Environment'Last),
                  Pattern => Terminator);
         return
           Environment (Key_Position + Key'Length ..  Terminator_Position - 1);
      exception
         when others =>
            System_Messages.Notify
              (Level   => Error,
               Message => "AGI: From_Environment: Can not extract " & Field &
                          " from the environment: """ & Environment & """");
            raise;
      end From_Environment;

      function Caller_ID is new From_Environment (Field => "callerid");
      function Context   is new From_Environment (Field => "context");
      function Extension is new From_Environment (Field => "extension");
   begin
      System_Messages.Notify (Debug, "AGI:");

      Check_Event :
      begin
         if Event = "AsyncAGI" then
            System_Messages.Notify
              (Debug, "AGI: Received an 'AsyncAGI' event.");
         else
            System_Messages.Notify
              (Error,
               "AGI: Received an '" & Event & "' event, which should have " &
                 "been sent elsewhere.");
            raise Program_Error;
         end if;
      end Check_Event;

      System_Messages.Notify (Level   => Debug,
                              Message => "AGI: Channel: " & Channel);

      case AGI_Event is
         when Start =>
            System_Messages.Notify
              (Debug, "AGI: Channel just opened.");

            System_Messages.Notify
              (Debug, "AGI: Telling Asterisk to answer.");
            PBX.Client.Send (AMI.Packet.Action.AGI
                               (Channel   => Channel,
                                Command   => "ANSWER",
                                CommandID => "fixed-1").To_AMI_Packet);

            System_Messages.Notify
              (Debug, "AGI: Telling Asterisk to answer.");
            PBX.Client.Send (AMI.Packet.Action.AGI
                               (Channel   => Channel,
                                Command   => "SAY ALPHA ""Hello " & Caller_ID &
                                             """ """"",
                                CommandID => "fixed-2").To_AMI_Packet);

            delay 5.0;

            System_Messages.Notify
              (Debug, "AGI: Telling Asterisk to hang up.");
            PBX.Client.Send (AMI.Packet.Action.AGI
                               (Channel   => Channel,
                                Command   => "HANGUP",
                                CommandID => "fixed-3").To_AMI_Packet);
         when Exec =>
            System_Messages.Notify
              (Debug,
               "AGI: Command " & Command_ID & " returned the result " &
                 Result & ".");
         when Unrecognised =>
            System_Messages.Notify
              (Debug, "AGI: Unrecognised (sub)event received.");
         when Close =>
            System_Messages.Notify
              (Debug, "AGI: Channel closed.");
      end case;
   exception
      when others =>
         System_Messages.Notify
           (Error,
            "AGI: Raised an exception.  Tell Jacob to do something about it.");
         raise;
   end Event;
begin
   AMI.Observers.Register (Event   => AMI.Event.AsyncAGI,
                           Handler => Event'Access);
end AGI.Callbacks;
