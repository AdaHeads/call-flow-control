-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               AMI.Action                                  --
--                                                                           --
--                                  BODY                                     --
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
with AMI.IO;
with AMI.Protocol;
with AWS.Net;
with Yolk.Log;

package body AMI.Action is
   use AWS.Net.Std;

   type Response_Type is
     (Ping,
     Get_Var);

   type Callback_Type is access procedure (Event_List : Event_List_Type.Map);
   type Callback_Routine_Table is array (Response_Type) of Callback_Type;

   CallBack_Routine : constant Callback_Routine_Table :=
     (others => null);

   Socket : Socket_Type;
   Logged_In : Boolean := False;

--     task body Action_Manager is
--        use Ada.Strings.Unbounded;
--        use Task_Controller;
--
--        Server_Host : Unbounded_String;
--        Server_Port : Positive;
--
--        Username    : Unbounded_String;
--        Secret      : Unbounded_String;
--
--        Greetings   : Unbounded_String;
--
--        subtype Fail_Counter_Type is Natural range 0 .. 3;
--        Fail_Counter : Fail_Counter_Type := 0;
--     begin
--  --        accept Initialize (Server_Host : in String;
--  --                           Server_Port : in Positive;
--  --                           Username    : in String;
--  --                           Secret      : in String) do
--  --
--  --         Action_Manager.Server_Host := To_Unbounded_String (Server_Host);
--  --           Action_Manager.Server_Port := Server_Port;
--  --
--  --           Action_Manager.Username := To_Unbounded_String (Username);
--  --           Action_Manager.Secret   := To_Unbounded_String (Secret);
--  --        end Initialize;
--
--        Reconnect :
--        loop
--           if Task_State = Up then
--              Yolk.Log.Trace (Yolk.Log.Info,
--                              "AMI Action_Manager is quitting");
--              exit Reconnect;
--           end if;
--           begin
--              AWS.Net.Std.Connect (Socket => AMI.Action.Socket,
--                                   Host   => To_String (Server_Host),
--                                   Port   => Server_Port);
--              Fail_Counter := 0;
--              Greetings := To_Unbounded_String
--                (AMI.IO.Read_Line (AMI.Action.Socket));
--              Yolk.Log.Trace (Yolk.Log.Debug,
--                              "Action Greeting: " & To_String (Greetings));
--
--              if not
--                AMI.Action.Login
--                  (Username => To_String (Action_Manager.Username),
--                   Secret   => To_String (Action_Manager.Secret))
--              then
--                 Yolk.Log.Trace (Yolk.Log.Notice,
--                                 "AMI Action Wrong login:" &
--                                 "Username: " &
--                                 To_String (Action_Manager.Username) &
--                                 "Secret: " &
--                                 To_String (Action_Manager.Secret));
--              end if;
--              Action_Loop :
--              loop
--                 if Task_State = Down then
--                    Yolk.Log.Trace (Yolk.Log.Info,
--                                    "AMI Action_Manager is quitting");
--                    exit Reconnect;
--                 end if;
--                 select
--                    accept Bridge (ChannelA : in String;
--                                   ChannelB : in String) do
--                       AMI.Action.Bridge (ChannelA, ChannelB);
--                    end Bridge;
--                 or
--                    accept CoreSettings (Data : out Event_List_Type.Map) do
--                       Data := AMI.Action.CoreSettings;
--                    end CoreSettings;
--                 or
--                    accept Get_Var (Channel      : in String;
--                                    VariableName : in String;
--                                    Value        : out Unbounded_String) do
--                       Value := To_Unbounded_String (
--                         AMI.Action.Get_Var (Channel      => Channel,
--                                             VariableName => VariableName));
--                    end Get_Var;
--                 or
--                    accept Hangup (Channel : in String) do
--                       AMI.Action.Hangup (Channel);
--                    end Hangup;
--                 or
--                    accept Login (Username : in     String;
--                                  Secret   : in     String;
--                                  Success  :    out Boolean) do
--                       Success := AMI.Action.Login (Username, Secret);
--                    end Login;
--                 or
--                    accept Logoff do
--                       AMI.Action.Logoff;
--                    end Logoff;
--                 or
--                    accept QueueStatus
--                    do
--                       Yolk.Log.Trace (Yolk.Log.Debug,
--                                       "Got into Accept QueueStatus");
--                       AMI.Action.QueueStatus;
--                    end QueueStatus;
--                 or
--                    accept Park (Channel1 : in String;
--                                 Channel2 : in String) do
--                       AMI.Action.Park (Channel1, Channel2);
--                    end Park;
--                 or
--                    accept Ping  do
--                       AMI.Action.Ping;
--                    end Ping;
--                 or
--                    accept Redirect (Channel : in String;
--                                     Exten   : in String;
--                                     Context : in String) do
--                       AMI.Action.Redirect (Channel, Exten, Context);
--                    end Redirect;
--                 or
--                    accept Set_Var (Channel      : in String;
--                                    VariableName : in String;
--                                    Value        : in String) do
--                       Yolk.Log.Trace (Yolk.Log.Debug,
--                                       "--==Set_Var_Channel:" & Channel);
--                       AMI.Action.Set_Var (Channel, VariableName, Value);
--                    end Set_Var;
--                 end select;
--              end loop Action_Loop;
--           exception
--              when others =>
--               Yolk.Log.Trace (Yolk.Log.Debug, "AMI Action Lost connection");
--                 Fail_Counter := Fail_Counter + 1;
--           end;
--           if Fail_Counter = 3 then
--              Yolk.Log.Trace (Yolk.Log.Info,
--                              "AMI-Action died because of failcounter");
--              exit Reconnect;
--           end if;
--
--           Yolk.Log.Trace (Yolk.Log.Debug, "AMI Action Reconnect");
--           delay 0.5;
--
--        end loop Reconnect;
--     exception
--        when E : others =>
--           Yolk.Log.Trace (Yolk.Log.Error,
--                           "Exception in Action_Manager: " &
--                             Ada.Exceptions.Exception_Name (E));
--     end Action_Manager;

   procedure Bridge (ChannelA : in String;
                     ChannelB : in String) is
      Command : constant String := Protocol.Bridge (ChannelA, ChannelB);
   begin
      AMI.IO.Send (Socket => Socket,
                   Item   => Command);
   end Bridge;

--     procedure CoreSettings is
--        Command : constant String := Protocol.CoreSettings;
--     begin
--        AMI.IO.Send (Socket, Command);
--     end CoreSettings;

   procedure Get_Var (Channel      : in String;
                      VariableName : in String) is
      use Ada.Strings.Unbounded;
      Command : constant String :=
        Protocol.Get_Var (Channel      => Channel,
                          VariableName => VariableName);
   begin
      AMI.IO.Send (Socket, Command);

--        declare
--           Event : constant Event_List_Type.Map := Read_Event_List;
--        begin
--           if not Event.Contains (To_Unbounded_String ("Response")) then
--              Yolk.Log.Trace (Yolk.Log.Debug, "AMI Action is out of Sync");
--              return "";
--           end if;
--
--           if To_String (Event.Element (To_Unbounded_String ("Response"))) /=
--             "Success" then
--              declare
--                 Message : Unbounded_String := To_Unbounded_String
--                     ("AMI-Action.Get_Var Exception without a Message");
--              begin
--                 if Event.Contains (To_Unbounded_String ("Message")) then
--                  Message := Event.Element (To_Unbounded_String ("Message"));
--                 end if;
--                 Yolk.Log.Trace (Yolk.Log.Debug,
--                                 "AMI-Action.Get_Var Errormessage:" &
--                                   To_String (Message));
--              end;
--              return "";
--           end if;
--
--           --  If the variable does not exsist
--           if Event.Contains (To_Unbounded_String ("Value")) then
--              if To_String (Event.Element (
--                To_Unbounded_String ("Value"))) = "(null)" then
--                 return "(null)";
--              end if;
--           end if;
--
--           declare
--              result : constant String :=
--                To_String (Event.Element (To_Unbounded_String ("Value")));
--           begin
--              Yolk.Log.Trace (Yolk.Log.Debug,
--                              "AMI.Action.Get_Var Command:" & Command &
--                             " Result: " & result);
--              return result;
--           end;
--
--        end;
   end Get_Var;

   procedure Hangup (Channel : in String) is
      use Ada.Strings.Unbounded;
      Command : constant String := Protocol.Hangup (Channel);
   begin
      Yolk.Log.Trace (Yolk.Log.Debug, "Hangup command [" & Command & "]");
      AMI.IO.Send (Socket, Command);
   end Hangup;

   --  Private Function
   function Login (Username : in String;
                   Secret   : in String) return Boolean is
      use Ada.Strings.Unbounded;
      Command : constant String := Protocol.Login (Username => Username,
                                                   Secret   => Secret);
   begin

--        Yolk.Log.Trace (Yolk.Log.Debug, "Log-In: " & Command);
      AMI.IO.Send (Socket, Command);

      declare
         Response : constant Event_Parser.Event_List_Type.Map :=
           Read_Event_List;
      begin
         --  Response: Success
         --  Message: Authentication accepted
         --  Yolk.Log.Trace (Yolk.Log.Debug,
         --   To_String (Response.First_Element));

         if
           To_String (Response.Element (To_Unbounded_String ("Response")))
           = "Success"
           and then
             To_String (Response.Element (To_Unbounded_String ("Message")))
           = "Authentication accepted" then

            return True;
         else
            Yolk.Log.Trace (Yolk.Log.Notice,
                            "AMI Action was not logged in with: " &
                              "Username: " & Username & " Secret: " & Secret);
            return False;
         end if;
      end;
   end Login;

   procedure Logoff is
      Command : constant String := Protocol.Logoff;
   begin
      AMI.IO.Send (Socket, Command);
   end Logoff;

   procedure Park (Channel          : in String;
                   Fallback_Channel : in String) is
      Command : constant String :=
        Protocol.Park (Channel          => Channel,
                       Fallback_Channel => Fallback_Channel);
   begin
      Yolk.Log.Trace (Yolk.Log.Debug, "AMI-Action.Park: " & Command);
      AMI.IO.Send (Socket, Command);
   end Park;

   procedure Ping is
      Command : constant String := Protocol.Ping;
   begin
      AMI.IO.Send (Socket, Command);
   end Ping;

--     function QueueStatus (ActionID : in String := "")
--                           return Call_List.Call_List_Type.Vector is
   procedure QueueStatus (ActionID : in String := "") is
      use Ada.Strings.Unbounded;
      Command : constant String := Protocol.QueueStatus (ActionID);
--        Response : Call_List.Call_List_Type.Vector;

--     function Extract_Total_Seconds (Text : in Unbounded_String)
--                                     return Duration;
--     function Extract_Total_Seconds (Text : in Unbounded_String)
--                                    return Duration is
--       Colon_Index : Integer;
--       Minutes : Unbounded_String;
--       Seconds : Unbounded_String;
--    begin
--       --  If there are any Colons, then the format is "mm:ss"
--       --  else it's "ssss"
--       --  m = minutes, s = seconds,
--       if Count (Text, ":") = 1 then
--          Colon_Index := Index (Text, ":");
--          --  Extract the minutes and Seconds
--          Minutes := Head (Source => Text, Count => Colon_Index - 1);
--          Seconds := Tail (Source => Text, Count =>
--                                Length (Text) - Colon_Index);
--          Yolk.Log.Trace (Yolk.Log.Debug, "Minutes: " & To_String (Minutes) &
--                            "Seconds: " & To_String (Seconds));
--          --  Convert Minutes to Seconds, and sum them.
--          return Duration'Value (To_String (Minutes)) * 60 +
--           Duration'Value (To_String (Seconds));
--      elsif Ada.Strings.Unbounded.Count (Text, ":") = 0 then
--         return Duration'Value (To_String (Text));
--      else
--         Yolk.Log.Trace (Yolk.Log.Warning,
--                         "Wait in QueueStatus has an unknown format: <" &
--                           To_String (Text) & ">.");
--          return 0.0;
--       end if;
--   end Extract_Total_Seconds;
   begin
      AMI.IO.Send (Socket, Command);
--        loop
--           declare
--              Event : constant Event_Parser.Event_List_Type.Map
--                := Read_Event_List;
--              Call : Call_List.Call_Type;
--           begin
--
--              if Event.Contains (To_Unbounded_String ("Event")) and then
--                To_String (Event.Element (To_Unbounded_String ("Event")))
--                = "QueueStatusComplete" then
--                 exit;
--
--                 --  Event: QueueEntry
--                 --  Queue: testqueue1
--                 --  Position: 1
--                 --  Channel: SIP/TP-Softphone-00000017
--                 --  Uniqueid: 1341827264.23
--                 --  CallerIDNum: TP-Softphone
--                 --  CallerIDName: unknown
--                 --  Wait: 98
--              elsif Event.Contains (To_Unbounded_String ("Event")) and then
--                To_String (Event.Element (To_Unbounded_String ("Event")))
--                = "QueueEntry" then
--
--                 if Event.Contains (To_Unbounded_String ("Queue")) then
--                    Call.Queue := Event.Element
--                      (To_Unbounded_String ("Queue"));
--                 end if;
--                 if Event.Contains (To_Unbounded_String ("Channel")) then
--                    Call.Channel := Event.Element
--                      (To_Unbounded_String ("Channel"));
--                 end if;
--                 if Event.Contains (To_Unbounded_String ("Uniqueid")) then
--                    Call.Uniqueid := Event.Element
--                      (To_Unbounded_String ("Uniqueid"));
--                 end if;
--                 if Event.Contains (To_Unbounded_String ("CallerIDNum")) then
--                    Call.CallerIDNum := Event.Element
--                      (To_Unbounded_String ("CallerIDNum"));
--                 end if;
--             if Event.Contains (To_Unbounded_String ("CallerIDName")) then
--                    Call.CallerIDName := Event.Element
--                      (To_Unbounded_String ("CallerIDName"));
--                 end if;
--                 if Event.Contains (To_Unbounded_String ("Position")) then
--                    Call.Position := Integer'Value (To_String (Event.Element
--                      (To_Unbounded_String ("Position"))));
--                 end if;
--                 --  elsif To_String (Key_Text) = "ActionID" then
--                 if Event.Contains (To_Unbounded_String ("Wait")) then
--                    declare
--                       use Ada.Calendar;
--                       Wait_Raw : Unbounded_String;
--                       Waited_In_Seconds : Duration;
--
--                  Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
--                   begin
--                   Wait_Raw := Event.Element (To_Unbounded_String ("Wait"));
--                      Waited_In_Seconds := Extract_Total_Seconds (Wait_Raw);
--                       Call.Arrived := Now - Waited_In_Seconds;
--                    exception
--                       when E : others =>
--                          Yolk.Log.Trace (Yolk.Log.Info,
--                            "Failed to parse QueueStatus Reponse" &
--                              "Wait: " & To_String (Event.Element (
--                              To_Unbounded_String ("Wait"))));
--                      Yolk.Log.Trace (Yolk.Log.Debug, "Action: QueueStatus" &
--                                       Ada.Exceptions.Exception_Name (E));
--                    end;
--                 end if;
--
--                 Response.Append (Call);
--              end if;
--           end;
--        end loop;

--        return Response;
   end QueueStatus;

   --  Private Function
   --  Reads an event, and parse it.
   function Read_Event_List return Event_Parser.Event_List_Type.Map is
      use Ada.Strings.Unbounded;
      package EP renames Event_Parser;

      Event_String : constant Unbounded_String :=
        AMI.IO.Read_Package (Socket);

      Event_List : constant EP.Event_List_Type.Map := EP.Parse (Event_String);
   begin
      return Event_List;
   end Read_Event_List;

   --  Response: Success
   --  Message: Redirect successful
   procedure Redirect (Channel : in String;
                       Exten   : in String;
                       Context : in String) is
      Command : constant String := Protocol.Redirect
        (Channel  => Channel,
         Context  => Context,
         Exten    => Exten,
         Priority => 1);
   begin
      AMI.IO.Send (Socket, Command);
   end Redirect;

   procedure Set_Var (Channel      : in String;
                      VariableName : in String;
                      Value        : in String) is
      use Ada.Strings.Unbounded;
      Command : constant String :=
        AMI.Protocol.Set_Var (Channel      => Channel,
                              VariableName => VariableName,
                              Value        => Value);
   begin
      AMI.IO.Send (Socket, Command);
      --  TODO Find out if this feature is nessesary.
      --       and what to do, if the request fails.
      --       possibility: Command Stack (First commit the stuff local,
      --                    when success)

      --  Action: Setvar
      --  Channel: SIP/TP-Softphone-00000000
      --  Variable: testings
      --  Value: This is now set

      --  Response: Success
      --  Message: Variable Set
   end Set_Var;

   procedure Start (Socket   : in AWS.Net.Std.Socket_Type;
                    Username : in String;
                    Secret   : in String) is
      use Ada.Strings.Unbounded;
      use AMI.IO;
   begin
      AMI.Action.Socket := Socket;

      --  Reads the grettings message
      --  Assumes that no one has read on the socket before.
      Yolk.Log.Trace (Yolk.Log.Debug, "Action Greetings: " &
                        AMI.IO.Read_Line (Socket));

      Logged_In := Login (Username => Username,
                          Secret   => Secret);

      if not Logged_In then
         Yolk.Log.Trace (Yolk.Log.Info, "AMI Action, failed to login");
         return;
      end if;

      loop
         declare
            Event_String : constant Unbounded_String := Read_Package (Socket);
            Event_List : Event_List_Type.Map;
            Event_Name : Response_Type;
         begin
            Event_List := Parse (Event_String);
            if Event_List.Contains (To_Unbounded_String ("Event")) then
               begin
                  --  TODO make a procedure for this. LESS UGLY.
                  --  Cast the event in String type to response type.
                  Event_Name := Response_Type'Value (To_String
                    (Event_List.Element
                       (To_Unbounded_String ("Event"))));
                  --  Calls the relevant function for that event.
                  CallBack_Routine
                    (Event_Name)(Event_List);

               exception
                     --  If it's not possible to cast the event name
                     --   to a Response type, then it trows an Constraint Error
                  when Constraint_Error =>
                     Yolk.Log.Trace (Yolk.Log.Error,
                       "Got an unknown Response Type in Action: "
                       & To_String (Event_List.Element
                         (To_Unbounded_String ("Event"))));
               end;
            end if;
         end;
      end loop;
   end Start;
end AMI.Action;
