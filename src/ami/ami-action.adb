with Ada.Exceptions,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

with AMI.IO,
     AMI.Protocol;

with Yolk.Log,
     Task_Controller;
package body AMI.Action is
   Socket : AWS.Net.Std.Socket_Type;

   task body Action_Manager is
      use Ada.Strings.Unbounded;
      use Task_Controller;

      Server_Host : Unbounded_String;
      Server_Port : Positive;

      Username : Unbounded_String;
      Secret : Unbounded_String;

      Greetings : Unbounded_String;
   begin
      accept Initialize (Server_Host : in String;
                         Server_Port : in Positive;
                         Username    : in String;
                         Secret      : in String) do

         Action_Manager.Server_Host := To_Unbounded_String (Server_Host);
         Action_Manager.Server_Port := Server_Port;

         Action_Manager.Username := To_Unbounded_String (Username);
         Action_Manager.Secret   := To_Unbounded_String (Secret);
      end Initialize;

      Reconnect :
      loop
         if Task_State = Down then
            Yolk.Log.Trace (Yolk.Log.Info,
                            "AMI Action_Manager is quitting");
            exit Reconnect;
         end if;
         begin
            AWS.Net.Std.Connect (Socket => AMI.Action.Socket,
                                 Host   => To_String (Server_Host),
                                 Port   => Server_Port);
            Greetings := To_Unbounded_String
              (AMI.IO.Read_Line (AMI.Action.Socket));
            Yolk.Log.Trace (Yolk.Log.Debug,
                            "Action Greeting: " & To_String (Greetings));

            if not
              AMI.Action.Login
                (Username => To_String (Action_Manager.Username),
                 Secret   => To_String (Action_Manager.Secret))
            then
               Yolk.Log.Trace (Yolk.Log.Notice, "AMI Action Wrong login:" &
                                 "Username: " &
                                 To_String (Action_Manager.Username) &
                                 "Secret: " &
                                 To_String (Action_Manager.Secret));
            end if;
            Action_Loop :
            loop
               if Task_State = Down then
                  Yolk.Log.Trace (Yolk.Log.Info,
                                  "AMI Action_Manager is quitting");
                  exit Reconnect;
               end if;
               select
                  accept Bridge (ChannelA : in String;
                                 ChannelB : in String) do
                     Bridge (ChannelA, ChannelB);
                  end Bridge;
               or
                  accept CoreSettings (Data : out Event_List_Type.Map) do
                     Data := AMI.Action.CoreSettings;
                  end CoreSettings;
               or
                  accept Login (Username : in     String;
                                Secret   : in     String;
                                Success  :    out Boolean) do
                     Success := Login (Username, Secret);
                  end Login;
               or
                  accept Logoff do
                     Logoff;
                  end Logoff;
               or
                  accept QueueStatus (List : out Call_List.Vector) do
                     List := AMI.Action.QueueStatus;
                  end QueueStatus;
               or
                  accept Park (Channel1 : in String;
                               Channel2 : in String) do
                     AMI.Action.Park (Channel1, Channel2);
                  end Park;
               or
                  accept Ping  do
                     AMI.Action.Ping;
                  end Ping;
               or
                  accept Redirect (Channel : in String;
                                   Exten   : in String;
                                   Context : in String) do
                     AMI.Action.Redirect (Channel, Exten, Context);
                  end Redirect;
               or
                  delay 1.0;
               end select;
            end loop Action_Loop;
         exception
            when others =>
               Yolk.Log.Trace (Yolk.Log.Debug, "AMI Action Lost connection");
         end;
         Yolk.Log.Trace (Yolk.Log.Debug, "AMI Action Reconnect");
         delay 0.5;

      end loop Reconnect;
   exception
      when E : others =>
         Yolk.Log.Trace (Yolk.Log.Error,
                         "Exception in Action_Manager: " &
                           Ada.Exceptions.Exception_Name (E));
   end Action_Manager;

   procedure Bridge (ChannelA : in String;
                     ChannelB : in String) is
      Command : constant String := Protocol.Bridge (ChannelA, ChannelB);
   begin
      AMI.IO.Send (Socket => Socket,
                   Item   => Command);
   end Bridge;

   function CoreSettings return Event_List_Type.Map is
      Command : constant String := Protocol.CoreSettings;
   begin
      AMI.IO.Send (Socket, Command);
      return Read_Event_List;
   end CoreSettings;

   procedure Initialize (Socket   : in AWS.Net.Std.Socket_Type;
                         Username : in String;
                         Secret   : in String) is
      Greetings : constant String := AMI.IO.Read_Line (Initialize.Socket);
   begin
      Yolk.Log.Trace (Yolk.Log.Debug, "Greetings: " & Greetings);
      AMI.Action.Socket := Socket;

      if not Login (Username, Secret) then
         null;
      end if;
   end Initialize;

   function Login (Username : in String;
                   Secret   : in String) return Boolean is
      use Ada.Strings.Unbounded;
      Command : constant String := Protocol.Login (Username => Username,
                                                   Secret   => Secret);
   begin

      Yolk.Log.Trace (Yolk.Log.Debug, "Log-In: " & Command);
      AMI.IO.Send (Socket, Command);

      declare
         Response : constant Event_Parser.Event_List_Type.Map :=
           Read_Event_List;
      begin
         --  Response: Success
         --  Message: Authentication accepted
         Yolk.Log.Trace (Yolk.Log.Debug, To_String (Response.First_Element));
         for item in Response.Iterate loop
            Yolk.Log.Trace (Yolk.Log.Debug,
                            To_String (Event_List_Type.Element (item)));
         end loop;
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

      Ada.Text_IO.Put_Line ("Parking Call");
      Ada.Text_IO.Put_Line (Command);
      AMI.IO.Send (Socket, Command);

   end Park;

   procedure Ping is
      Command : constant String := Protocol.Ping;
   begin
      AMI.IO.Send (Socket, Command);

      --  read Pong
      declare
         use Ada.Strings.Unbounded;
         Pong : constant Event_Parser.Event_List_Type.Map := Read_Event_List;
      begin
         Yolk.Log.Trace (Yolk.Log.Debug,
           To_String (Pong.Element
             (To_Unbounded_String ("Ping"))));
      end;
   end Ping;

   function QueueStatus (ActionID : in String := "") return Call_List.Vector is
      use Ada.Strings.Unbounded;
      Command : constant String := Protocol.QueueStatus (ActionID);
      Response : Call_List.Vector;
      --        Key_Text, Value_Text : Unbounded_String;
   begin

      AMI.IO.Send (Socket, Command);
      loop
         declare
            Event : constant Event_Parser.Event_List_Type.Map
              := Read_Event_List;
            Call : Call_Queue.Call_Type;
         begin

            if Event.Contains (To_Unbounded_String ("Event")) and then
              To_String (Event.Element (To_Unbounded_String ("Event")))
              = "QueueStatusComplete" then
               exit;
            elsif Event.Contains (To_Unbounded_String ("Event")) and then
              To_String (Event.Element (To_Unbounded_String ("Event")))
              = "QueueEntry" then

               --                 Extract_Call_Info :
               --                 for i in Event'Range loop
               --                    Key_Text := Event (i, Key);
               --                    Value_Text := Event (i, Value);
               if Event.Contains (To_Unbounded_String ("Queue")) then
                  Call.Queue := Event.Element
                    (To_Unbounded_String ("Queue"));
               end if;
               if Event.Contains (To_Unbounded_String ("Channel")) then
                  Call.Channel := Event.Element
                    (To_Unbounded_String ("Channel"));
               end if;
               if Event.Contains (To_Unbounded_String ("Uniqueid")) then
                  Call.Uniqueid := Event.Element
                    (To_Unbounded_String ("Uniqueid"));
               end if;
               if Event.Contains (To_Unbounded_String ("CallerIDNum")) then
                  Call.CallerIDNum := Event.Element
                    (To_Unbounded_String ("CallerIDNum"));
               end if;
               if Event.Contains (To_Unbounded_String ("CallerIDName")) then
                  Call.CallerIDName := Event.Element
                    (To_Unbounded_String ("CallerIDName"));
               end if;
               if Event.Contains (To_Unbounded_String ("Position")) then
                  Call.Position := Integer'Value (To_String (Event.Element
                    (To_Unbounded_String ("Position"))));
               end if;
               --  elsif To_String (Key_Text) = "ActionID" then
               --     if Event.Contains (To_Unbounded_String ("Wait")) then
               --        declare
               --           use Ada.Calendar;
               --           Waited_In_Seconds : Duration;
               --       Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
               --        begin
               --           Waited_In_Seconds := Duration'Value
               --                   (To_String (Event.Element (
               --                        To_Unbounded_String ("Wait"))));
               --                     Call.Arrived := Now - Waited_In_Seconds;
               --                    exception
               --                       when others =>
               --                        Yolk.Log.Trace (Yolk.Log.Info,
               --                     "Failed to parse QueueStatus Reponse" &
               --                        "Wait: " & To_String (Event.Element (
               --                            To_Unbounded_String ("Wait"))));
               --                    end;
               --                 end if;
               --                 end loop Extract_Call_Info;
               Response.Append (Call);
            end if;
         end;
      end loop;

      return Response;
   end QueueStatus;

   --  Event: QueueEntry
   --  Queue: testqueue1
   --  Position: 1
   --  Channel: SIP/TP-Softphone-00000017
   --  Uniqueid: 1341827264.23
   --  CallerIDNum: TP-Softphone
   --  CallerIDName: unknown
   --  Wait: 98

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

      declare
         use Ada.Strings.Unbounded;
         Response : constant Event_List_Type.Map := Read_Event_List;
      begin
         if Response.Contains (To_Unbounded_String ("Message")) and then
           To_String (Response.Element (To_Unbounded_String ("Message"))) =
           "Redirect successful" then
            null;
         end if;
      end;
   end Redirect;
end AMI.Action;
