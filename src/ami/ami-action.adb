--  with AMI.Event; use AMI.Event;
with AMI.IO;
with Ada.Text_IO;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Yolk.Log;
package body AMI.Action is
   Socket : AWS.Net.Std.Socket_Type;

   --  Source: http://rosettacode.org/wiki/Mutex
   protected Mutex is
      entry Seize;
      procedure Release;
   private
      Owned : Boolean := False;
   end Mutex;

   protected body Mutex is
      entry Seize when not Owned is
      begin
         Owned := True;
      end Seize;
      procedure Release is
      begin
         Owned := False;
      end Release;
   end Mutex;

   procedure Bridge (ChannelA : in String;
                     ChannelB : in String) is
      Command : constant String := Protocol.Bridge (ChannelA, ChannelB);
   begin
      Mutex.Seize;

      AMI.IO.Send (Socket => Socket,
                   Item   => Command);

      Mutex.Release;
   end Bridge;

   function CoreSettings return String is
      use Ada.Strings.Unbounded;
      Command : constant String := Protocol.CoreSettings;
      Response : Unbounded_String := Null_Unbounded_String;
   begin
      Mutex.Seize;

      AMI.IO.Send (Socket, Command);
      declare
         Event : constant Event_List_Type := Read_Event_List;
      begin
         for i in Event'Range loop
            if To_String (Event (i, Key)) = "AsteriskVersion" then
               Response := Event (i, Value);
            end if;
         end loop;
      end;

      Mutex.Release;

      if Response /= Null_Unbounded_String then
         return To_String (Response);
      else
         return "Error";
      end if;
   end CoreSettings;

   procedure Initialize (Socket   : in AWS.Net.Std.Socket_Type;
                         Username : in String;
                         Secret   : in String) is
      Greetings : constant String := AMI.IO.Read_Line (Initialize.Socket);
   begin
      Yolk.Log.Trace (Yolk.Log.Debug, Greetings);
      AMI.Action.Socket := Socket;

      Login (Username, Secret);
   end Initialize;

   procedure Login (Username : in String;
                    Secret   : in String) is
      use Ada.Strings.Unbounded;
      Command : constant String := Protocol.Login (Username => Username,
                                                   Secret   => Secret);
   begin
      Mutex.Seize;

      Yolk.Log.Trace (Yolk.Log.Debug, Command);
      AMI.IO.Send (Socket, Command);

      declare
         Response : constant Event_Parser.Event_List_Type := Read_Event_List;
      begin
         --  Response: Success
         --  Message: Authentication accepted
         if
           To_String (Response (Response'First, Event_Parser.Value))
           /= "Success"
           or else
             To_String (Response (Response'First + 1, Event_Parser.Value))
           /= "Authentication accepted"
         then
            Yolk.Log.Trace (Yolk.Log.Notice,
                            "AMI Action was not logged in with: " &
                            "Username: " & Username & " Secret: " & Secret);
         end if;
      end;

      Mutex.Release;
   end Login;

   procedure Logoff is
      Command : constant String := Protocol.Logoff;
   begin
      Mutex.Seize;

      AMI.IO.Send (Socket, Command);

      Mutex.Release;
   end Logoff;

   procedure Park (Channel          : in String;
                   Fallback_Channel : in String) is
      Command : constant String :=
        Protocol.Park (Channel          => Channel,
                       Fallback_Channel => Fallback_Channel);
   begin
      Mutex.Seize;

      Ada.Text_IO.Put_Line ("Parking Call");
      Ada.Text_IO.Put_Line (Command);
      AMI.IO.Send (Socket, Command);

      Mutex.Release;
   end Park;

   procedure Ping is
      Command : constant String := Protocol.Ping;
   begin
      Mutex.Seize;
      AMI.IO.Send (Socket, Command);

      --  read Pong
      declare
         use Ada.Strings.Unbounded;
         Pong : constant Event_Parser.Event_List_Type := Read_Event_List;
      begin
         Yolk.Log.Trace (Yolk.Log.Debug,
           To_String (Pong (Pong'First + 1,
                      Event_Parser.Value)));
      end;

      Mutex.Release;
   end Ping;

   procedure QueuePause (DeviceName : in String;
                         State      : in Protocol.Pause_States) is
      Command : constant String :=
        Protocol.QueuePause
          (DeviceName => DeviceName,
           State      => State);
   begin
      Mutex.Seize;

      AMI.IO.Send (Socket, Command);

      Mutex.Release;
   end QueuePause;

   function QueueStatus (ActionID : in String := "") return Call_List.Vector is
      use Ada.Strings.Unbounded;
      Command : constant String := Protocol.QueueStatus (ActionID);
      Response : Call_List.Vector;
      Key_Text, Value_Text : Unbounded_String;
   begin
      Mutex.Seize;

      AMI.IO.Send (Socket, Command);
      loop
         declare
            Event : constant Event_Parser.Event_List_Type := Read_Event_List;
            Call : Call_Queue.Call_Type;
         begin

            if To_String (Event (Event'First, Key)) = "Event" and then
              To_String (Event (Event'First, Value))
              = "QueueStatusComplete" then
               exit;
            elsif To_String (Event (Event'First, Key)) = "Event" and then
              To_String (Event (Event'First, Value)) = "QueueEntry" then

               Extract_Call_Info :
               for i in Event'Range loop
                  Key_Text := Event (i, Key);
                  Value_Text := Event (i, Value);
                  if To_String (Key_Text) = "Queue" then
                     Call.Queue := Value_Text;
                  elsif To_String (Key_Text) = "Channel" then
                     Call.Channel := Value_Text;
                  elsif To_String (Key_Text) = "Uniqueid" then
                     Call.Uniqueid := Value_Text;
                  elsif To_String (Key_Text) = "CallerIDNum" then
                     Call.CallerIDNum := Value_Text;
                  elsif To_String (Key_Text) = "CallerIDName" then
                     Call.CallerIDName := Value_Text;
                  elsif To_String (Key_Text) = "Position" then
                     Call.Position := Integer'Value (To_String (Value_Text));
                     --  elsif To_String (Key_Text) = "ActionID" then
                  elsif To_String (Key_Text) = "Wait" then
                     declare
                        use Ada.Calendar;
                        Waited_In_Seconds : Duration;
                        Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
                     begin
                        Waited_In_Seconds := Duration'Value
                          (To_String (Value_Text));
                        Call.Arrived := Now - Waited_In_Seconds;
                     exception
                        when others =>
                           Ada.Text_IO.Put_Line
                             ("QueueEntry: wait is not an Duration," &
                                "and that is wrong: " &
                                To_String (Value_Text));
                     end;
                  end if;
               end loop Extract_Call_Info;
               Response.Append (Call);
            end if;
         end;
      end loop;

      Mutex.Release;
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

   function Read_Event_List return Event_Parser.Event_List_Type is
      use Ada.Strings.Unbounded;
      package EP renames Event_Parser;

      Event_String : constant Unbounded_String :=
        AMI.IO.Read_Package (Socket);

      Event_List : constant EP.Event_List_Type := EP.Parse (Event_String);
   begin
      return Event_List;
   end Read_Event_List;

   --  Response: Success
   --  Message: Redirect successful
   procedure Redirect (Channel : in String;
                       Exten   : in String;
                       Context : in String := "LocalSets") is
      Command : constant String := Protocol.Redirect
        (Channel  => Channel,
         Context  => Context,
         Exten    => Exten,
         Priority => 1);
   begin
      Mutex.Seize;

      AMI.IO.Send (Socket, Command);

      declare
         use Ada.Strings.Unbounded;
         Response : constant Event_List_Type := Read_Event_List;
      begin
         if To_String (Response (Response'Last, Event_Parser.Value)) =
           "Redirect successful" then
            null;
         end if;
      end;
      Mutex.Release;
   end Redirect;
end AMI.Action;
