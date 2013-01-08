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

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with AWS.URL;

with AMI.Event;
with AMI.Observers;
with AMI.Trace;
with AMI.Packet.Action;
with PBX;

package body AGI.Callbacks is

   -------------
   --  Event  --
   -------------

   procedure Event
     (Packet : in AMI.Parser.Packet_Type)
   is
      use AMI.Trace;

      type AGI_Events is (Start, Exec, Close, Unrecognised);

      function AGI_Event
        return AGI_Events;
      --  TODO: Write comment

      function Channel
        return String;
      --  TODO: Write comment

      function Command_ID
        return String;
      --  TODO: Write comment

      function Event
        return String;
      --  TODO: Write comment

      generic
         Field : String;
      function From_Environment
        return String;
      --  TODO: Write comment

      function Result
        return String;
      --  TODO: Write comment

      function Value
        (Key : in AMI.Parser.AMI_Key_Type)
         return String;
      --  TODO: Write comment

      -----------------
      --  AGI_Event  --
      -----------------

      function AGI_Event
        return AGI_Events
      is
         Descriptor : constant String := Value (AMI.Parser.SubEvent);
      begin
         if Descriptor = "End" then
            return Close;
         else
            return AGI_Events'Value (Descriptor);
         end if;
      exception
         when Constraint_Error =>
            AMI.Trace.Log
              (Error,
               "AGI: Received an '" &
                 Descriptor &
                 "' subevent.  " &
                 "Tell Jacob to add it to type AGI_Events.");

            return Unrecognised;
      end AGI_Event;

      ---------------
      --  Channel  --
      ---------------

      function Channel
        return String
      is
      begin
         return Value (AMI.Parser.Channel);
      end Channel;

      ------------------
      --  Command_ID  --
      ------------------

      function Command_ID
        return String
      is
      begin
         return Value (AMI.Parser.CommandID);
      end Command_ID;

      -------------
      --  Event  --
      -------------

      function Event
        return String
      is
         use Ada.Strings.Unbounded;
      begin
         return To_String (Packet.Header.Value);
      end Event;

      ------------------------
      --  From_Environment  --
      ------------------------

      function From_Environment
        return String
      is
         use Ada.Characters;
         use Ada.Strings.Fixed;

         Key                 : constant String := "agi_" & Field & ": ";
         Terminator          : constant String := (1 => Latin_1.LF);
         Environment         : constant String := Value (AMI.Parser.Env);
         Key_Position        : Positive;
         Terminator_Position : Positive;
      begin
         Key_Position :=
           Index (Source  => Environment,
                  Pattern => Key);

         Terminator_Position := Index
           (Source  => Environment
              (Key_Position + Key'Length .. Environment'Last),
            Pattern => Terminator);

         return Environment
           (Key_Position + Key'Length ..  Terminator_Position - 1);
      exception
         when others =>
            AMI.Trace.Log
              (Error,
               "AGI: From_Environment: Can not extract " &
                 Field &
                 " from the environment: """ &
                 Environment &
                 """");

            raise;
      end From_Environment;

      --------------
      --  Result  --
      --------------

      function Result
        return String
      is
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
            AMI.Trace.Log
              (Error,
               "AGI: Result field could not be parsed: """ &
                 Raw &
                 """.  Give Jacob a copy of the raw result string.");

            return Raw;
      end Result;

      -------------
      --  Value  --
      -------------

      function Value
        (Key : in AMI.Parser.AMI_Key_Type)
         return String
      is
         use Ada.Strings.Unbounded;
      begin
         return AWS.URL.Decode (To_String (Packet.Fields.Element (Key)));
      exception
         when Constraint_Error =>
            AMI.Trace.Log
              (Error,
               "AGI: No " &
                 AMI.Parser.AMI_Key_Type'Image (Key) &
                 " value in packet.");

            return "";
      end Value;

      function Caller_ID is new From_Environment (Field => "callerid");
      function Context   is new From_Environment (Field => "context");
      function Extension is new From_Environment (Field => "extension");

      pragma Unreferenced (Context, Extension);
   begin
      AMI.Trace.Log (Debug, "AGI:");

      Check_Event :
      begin
         if Event = "AsyncAGI" then
            AMI.Trace.Log
              (Debug, "AGI: Received an 'AsyncAGI' event.");
         else
            AMI.Trace.Log
              (Error,
               "AGI: Received an '" &
                 Event &
                 "' event, which should have " &
                 "been sent elsewhere.");

            raise Program_Error;
         end if;
      end Check_Event;

      AMI.Trace.Log (Debug, "AGI: Channel: " & Channel);

      case AGI_Event is
         when Start =>
            AMI.Trace.Log (Debug, "AGI: Channel just opened.");
            AMI.Trace.Log (Debug, "AGI: Telling Asterisk to answer.");

            PBX.Client.Send
              (AMI.Packet.Action.AGI
                 (Channel   => Channel,
                  Command   => "ANSWER",
                  CommandID => "fixed-1").To_AMI_Packet);

            if Caller_ID = "TL-Softphone" then
               AMI.Trace.Log (Debug, "AGI: Say 'bye' to Thomas. ;-)");

               PBX.Client.Send
                 (AMI.Packet.Action.AGI
                    (Channel   => Channel,
                     Command   => "SAY ALPHA ""Bye"" """"",
                     CommandID => "fixed-2").To_AMI_Packet);
            elsif Caller_ID = "softphone1" then
               AMI.Trace.Log (Debug, "AGI: Send Jacob to the IVR.");

               PBX.Client.Send
                 (AMI.Packet.Action.AGI
                    (Channel   => Channel,
                     Command   => "EXEC READ LifeTheUniverseAndEverything," &
                       """vm-press""&""digits/4""&""digits/2"",2",
                     CommandID => "IVR").To_AMI_Packet);

               PBX.Client.Send
                 (AMI.Packet.Action.AGI
                    (Channel   => Channel,
                     Command   => "GET VARIABLE LifeTheUniverseAndEverything",
                     CommandID => "get_variable").To_AMI_Packet);

               PBX.Client.Send
                 (AMI.Packet.Action.AGI
                    (Channel   => Channel,
                     Command   => "STREAM FILE ""demo-thanks"" """"",
                     CommandID => "fixed-2c").To_AMI_Packet);
            else
               AMI.Trace.Log (Debug, "AGI: Queue call.");

               PBX.Client.Send
                 (AMI.Packet.Action.AGI
                    (Channel   => Channel,
                     Command   => "EXEC QUEUE org_id1",
                     CommandID => "fixed-2").To_AMI_Packet);
            end if;

            AMI.Trace.Log
              (Debug, "AGI: Telling Asterisk to hang up.");

            PBX.Client.Send
              (AMI.Packet.Action.AGI
                 (Channel   => Channel,
                  Command   => "HANGUP",
                  CommandID => "fixed-3").To_AMI_Packet);
         when Exec =>
            if Command_ID = "get_variable" then
               AMI.Trace.Log
                 (Debug, "AGI: Value request: " & Value (AMI.Parser.Result));
            else
               AMI.Trace.Log
                 (Debug,
                  "AGI: Command " &
                    Command_ID &
                    " returned the result " &
                    Result &
                    ".");
            end if;
         when Unrecognised =>
            AMI.Trace.Log
              (Debug, "AGI: Unrecognised (sub)event received.");
         when Close =>
            AMI.Trace.Log (Debug, "AGI: Channel closed.");
      end case;
   exception
      when others =>
         AMI.Trace.Log
           (Error,
            "AGI: Raised an exception.  Tell Jacob to do something about it.");
         raise;
   end Event;
begin
   AMI.Observers.Register (Event   => AMI.Event.AsyncAGI,
                           Handler => Event'Access);
end AGI.Callbacks;
