-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Routines                                  --
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

with Ada.Exceptions;

with AMI.Response;
with AMI.Client;

with Model.Call;
with Peers;
with System_Messages;

package body AMI.Action is
   use System_Messages;
   
   procedure Login
     (Client   : access Client_Type;
      Username : in     String;
      Secret   : in     String;
      Callback : in     AMI.Callback.Callback_Type 
	:= AMI.Callback.Login_Callback'Access) is
      Action_ID : constant Action_ID_Type := 
	Protocol_Strings.Next_Action_ID;
   begin
      AMI.Response.Subscribe (Action_ID,Callback);
      System_Messages.Notify (Debug, "AMI.Client.Login: Subscribed for " &
				"a reply with ActionID" & Action_ID'Img);
      AMI.Client.Send (Client => Client, 
		       Item   => Protocol_Strings.Login 
			 (Username  => Username,
			  Secret    => Secret,
			  Action_ID => Action_ID));
   end Login;
   
   procedure Ping (Client   : access Client_Type;
		   Callback : in     AMI.Callback.Callback_Type 
		     := AMI.Callback.Ping_Callback'Access) is
      Action_ID : constant Action_ID_Type := 
	Protocol_Strings.Next_Action_ID;
   begin
      AMI.Response.Subscribe (Action_ID,Callback);
      System_Messages.Notify (Debug, "AMI.Client.Ping: Subscribed for " &
				"a reply with ActionID" & Action_ID'Img);
      AMI.Client.Send (Client => Client, 
		       Item   => Protocol_Strings.Ping 
			 (Action_ID => Action_ID));
  end Ping;
  
  procedure Bridge (Client   : access Client_Type;
		    ChannelA : in     String;
		    ChannelB : in     String;
		    Callback : in     AMI.Callback.Callback_Type 
		      := AMI.Callback.Null_Callback'Access) is
     Action_ID : constant Action_ID_Type := 
       Protocol_Strings.Next_Action_ID;
  begin
     AMI.Response.Subscribe (Action_ID,Callback);
     AMI.Client.Send 
       ( Client => Client,
	 Item   => Protocol_Strings.Bridge 
	   (Channel1 => ChannelA, 
	    Channel2 => ChannelB,
	    Action_ID => Action_ID));
  end Bridge;
   
  
  procedure Hangup (Client  : access Client_Type;
		    Call_Id : in     Unbounded_String;
		    Status  :    out Status_Type) is
     use Peers;
     use Model.Call;

     Call : Call_Type;
     Action_ID : constant Action_ID_Type := 
       Protocol_Strings.Next_Action_ID;
  begin
     System_Messages.Notify (Debug, "Hangup: routine started");
     Status := Unknown_Error;

     Call := Get_Call (UniqueID => Call_Id);

     if Call = Null_Call then
	Status := No_Call_Found;
     end if;

     System_Messages.Notify (Debug,
			     "Hangup Call: " & To_String (Call_Id));

     AMI.Client.Send 
       ( Client => Client,
	 Item   => Protocol_Strings.Hangup
	   (Ada.Strings.Unbounded.To_String (Call.Channel), Action_ID));
     

     Status := Success;

     System_Messages.Notify (Debug, "The Hangup routine is done.");
  end Hangup;
  
  --  Get the specific call with UniqueId matching
  --  If unitqueID is null, then the first call in the queue is taken.
  procedure Get_Call (Client    : access Client_Type;
		      Unique_Id : in     String;
		      Agent_Id  : in     String;
		      Status    :    out Status_Type) is
     use Model.Call;
     use Peers;
     use Peers.Peer_List_Type;

     Temp_Call : Call_Type;
     Peer      : Peer_Type;
     
     Action_ID : constant Action_ID_Type := 
       Protocol_Strings.Next_Action_ID;
  begin
     Status := Unknown_Error;
     --        Peer := Peers.Get_Peer (Agent_ID => To_Unbounded_String (Agent_Id));
     Peer := Peers.Get_Peer_By_PhoneName (To_Unbounded_String (Agent_Id));

     --  Check if there exsist an Agent by that name.
     if Peer = Null_Peer then
	System_Messages.Notify (Debug, "Get_Call: " &
				  "We have no agent registred by that ID: [" &
				  Agent_Id & "]");
	Status := No_Agent_Found;
	return;
     end if;

     --  Now that we know that there exists a Peer -
     --  But is the SIP phone registered?
     System_Messages.Notify (Debug, "Get_Call - Agent_ID: " & Agent_Id &
			       " ask for Call_ID: " & Unique_Id);
     if Peer.State = Unregistered then
	System_Messages.Notify (Debug, "Get_Call: " &
				  "The following agent is unregistred: " & Agent_Id);
	Status := Unregistered_Agent;
	return;
     end if;

     Temp_Call := Get_Call (UniqueID =>
					To_Unbounded_String (Unique_Id));

     if Temp_Call = Null_Call then
	System_Messages.Notify (Debug,
				"Get_Call: No Call to take with ID: " & Unique_Id);
	Status := No_Call_Found;
     end if;
     System_Messages.Notify (Debug, "Pickup Call - Channel: " & To_String (Temp_Call.Channel));
     
     --  If there is a call to anwser.
     Status := Success;

     --  Send the call out to the phone
     AMI.Client.Send 
       ( Client => Client,
	 Item   =>      Protocol_Strings.Redirect
	   (Channel   => To_String (Temp_Call.Channel),
	    Exten     => To_String (Peer.Exten),
	    Context   => "LocalSets",
	    Action_ID => Action_ID));

  exception
     when Error : others =>
	System_Messages.Notify (Debug,
				"Got and Exception in Get_Call");
	System_Messages.Notify (Debug,
				Ada.Exceptions.Exception_Information (Error));
  end Get_Call;
  
  
--  --  Takes two channels, and bridge the them together.
--     procedure Bridge_Call (Call_Id_1 : in     Unbounded_String;
--                            Call_Id_2 : in     Unbounded_String;
--                            Status    :    out Status_Type) is
--        use Call_List;
--        Action_ID : constant Action_ID_Type := 
--  	Protocol_Strings.Next_Action_ID;
--        Call1, Call2 : Call_Type;
--     begin
--        Status := Unknown_Error;
--        Call1 := Call_List.Get_Call (Call_Id_1);
--        Call2 := Call_List.Get_Call (Call_Id_2);

--        if Call1 = Null_Call or else Call2 = Null_Call then
--           Status := No_Call_Found;
--        end if;

--        --  Sends the command out to Asterisk.
--        AMI.Client.Send (Item =>
--  			 Protocol_Strings.Bridge
--  			 (To_String (Call1.Channel), To_String (Call2.Channel), Action_ID);
--  		       Status := Success;
--  		       end Bridge_Call;

--     --     procedure Consistency_Check is
--     --        use Call_List;
--     --        use Ada.Containers;
--     --
--     --        Temp_Call_List : Call_List_Type.Vector;
--     --
--     --        Queue : constant Call_List_Type.Vector := Call_List.Get;
--     --        function Check_Call (Call : in Call_Type) return Boolean;
--     --        function Check_Call (Call : in Call_Type) return Boolean is
--     --        begin
--     --           for Queue_Priority in Queue'Range loop
--     --              for Queue_Index in
--     --                1 .. Integer (Queue (Queue_Priority).Length) loop
--     --                 if Call.Uniqueid =
--     --                   Queue (Queue_Priority).Element (Queue_Index).Uniqueid
--     --                 then
--     --                    return True;
--     --                 end if;
--     --              end loop;
--     --           end loop;
--     --           return False;
--     --        end Check_Call;
--     --     begin
--     --        AMI.Action.Action_Manager.QueueStatus (Call_List);
--     --        --  AMI.Action.QueueStatus (Asterisk.Channel, "Consistency");
--     --        System_Messages.Log (Yolk.Log.Debug, "Consistency Check");
--     --        if Call_List.Queue_Length /= Call_List.Length then
--     --           System_Messages.Log (Yolk.Log.Info,
--     --           "Consistency check - Length failed" &
--     --           "Call Queue Length: " & Call_List.Queue_Length'Img &
--     --           "Asterisk Queue Length: " & Call_List.Length'Img);
--     --        end if;
--     --        --  TODO Error Correction
--     --        --  XXX Der er en chance for det her er langsomst.
--     --
--     --        for Cons_Index in 1 .. Integer (Call_List.Length) loop
--     --           if not Check_Call (Call_List.Element (Cons_Index)) then
--     --              System_Messages.Log (Yolk.Log.Info,
--     --                              "Consistency check - Not Equal failed" &
--     --                         Call_To_String (Call_List.Element (Cons_Index)) &
--     --                              "Does not exsist in our call queue");
--     --           end if;
--     --        end loop;
--     --        System_Messages.Log (Yolk.Log.Debug, "The system is consistent");
--     --     end Consistency_Check;


  --     --        --  Scaffolding
  --     --     procedure Get_Version is --  return String is
--     --        use Event_Parser;
--     --        Data : Event_List_Type.Map;
--     --     begin
--     --        AMI.Action.CoreSettings (Data);
--     --        if Data.Contains (To_Unbounded_String ("AsteriskVersion")) then
--     --      Version := Data.Element (To_Unbounded_String ("AsteriskVersion"));
--     --        end if;
--     --        return To_String (Version);
--     --     end Get_Version;


  --  TODO Refactor, in case something goes wrong,
  --    the steps are taken in the wrong order.
  --  Sends the agent's current call on hold / park.
  procedure Park (Client  : access Client_Type;
		  Call_Id : in     String;
		  Status  :    out Status_Type) is
     use Peers;
     use Peers.Peer_List_Type;
     use Model.Call;

     Call : Call_Type;
     Action_ID : constant Action_ID_Type := 
       Protocol_Strings.Next_Action_ID;
  begin
     --  Default fallback status.
     Status := Unknown_Error;

     --  Finds the Agent, to get the call to park.
     --   Peer := Peers.Get_Peer (Agent_ID => To_Unbounded_String (Agent_ID));
     --        if Peer = Peers.null_Peer then
     --           --  No peer found
     --           Status := No_Agent_Found;
     --           System_Messages.Notify (Debug,
     --                           "Park Routine: The agent does not exsist: "
     --                           & Agent_ID);
     --           return;
     --        elsif Peer.Status = Unregistered then
     --           Status := Unregistred_Agent;
     --           System_Messages.Notify (Debug,
     --                           "Park Routine: The agent does not exsist: "
     --                           & Agent_ID);
     --           return;
     --        end if;

     Call := Get_Call (UniqueID => To_Unbounded_String (Call_Id));

     if Call = Null_Call then
	System_Messages.Notify (Debug,
				"Park_Call: Call not found, ID: " & Call_Id);
	Status := No_Call_Found;
	return;
     end if;

     System_Messages.Notify (Debug,
			     "Park: Action send, Call_ID => " & Call_Id);

     --  Move the call back to the Queue, which will act like a parking lot.

     --  TODO Update, There have to be an easier way
     --   to find the companies Extension.
     --  Get the extension the queue is tied up to.

     --           AMI.Action.Action_Manager.Get_Var
     --             (Channel      => To_String (Call.Channel),
     --              VariableName => "Extension",
     --              Value        => Exten);

     if Call.Extension = Null_Unbounded_String or else
       To_String (Call.Extension) = "(null)" then
	System_Messages.Notify (Debug,
				"Routines.Park: " &
				  "The Call does not have an Extension Variable" &
				  "Call ID: " & To_String (Call.Uniqueid));
	Status := Unknown_Error;
	return;
     elsif To_String (Call.Extension) = "" then
	System_Messages.Notify (Debug,
				"Routines.Park: " &
				  "The Call have an empty extension variable" &
				  "Call ID: " & To_String (Call.Uniqueid));
	Status := Unknown_Error;
	return;
     end if;

     --  Sets the variable that tells this call is a call on hold.
     --           AMI.Action.Action_Manager.Set_Var
     --             (Channel      => To_String (Call.Channel),
     --              VariableName => "CallState",
     --              Value        => "onhold");

     --  Move the call back to the queue
     
     AMI.Client.Send 
       ( Client => Client,
	 Item   =>      Protocol_Strings.Redirect
	   (Channel   => To_String (Call.Channel),
	    Exten     => To_String (Call.Extension),
	    Context   => "LocalSets",
	    Action_ID => Action_ID));
     --  Update peer information
     --        Peer.State := free;
     --        Peers.Replace_Peer (Peer);

     Status := Success;
  end Park;

  --     procedure Register_Agent (Phone_Name  : in Unbounded_String;
  --                               Computer_Id : in Unbounded_String) is
  --        use Peers;
  --        Peer       : Peer_Type;
  --        Peer_Index : Peer_List_Type.Cursor;
  --        Peer_List  : constant Peer_List_Type.Map := Peers.Get_Peers_List;
--     begin
--        Peer_Index := Peer_List_Type.Find (Container => Peer_List,
--                                           Key       => Phone_Name);
--        if Peer_List_Type.Has_Element (Peer_Index) then
--           --  The phone allready exsist in the list.
--           Peer := Peer_List_Type.Element (Position => Peer_Index);
--           Peer.Computer_ID := Computer_Id;
--           Peers.Replace_Peer (Item => Peer);
--        else
--           Peer.Peer := Phone_Name;
--           Peer.ChannelType := To_Unbounded_String ("SIP");
--           Peer.Computer_ID := Computer_Id;
--           Peers.Insert_Peer (New_Item  => Peer);
--        end if;
--     end Register_Agent;

--     procedure Startup_Sequence is
--     --        Call_List : AMI.Action.Call_List.Vector;
--     begin
--        System_Messages.Notify (Debug, "Calling QueueStatus");
--        AMI.Action.QueueStatus;
--        --        for i in Call_List.First_Index .. Call_List.Last_Index loop
--        --           Call_List.Enqueue (Call => Call_List.Element (i));
--        --        end loop;
--     end Startup_Sequence;

--     procedure Test_Status_Print is
--        use Peers;
--     begin
--        System_Messages.Notify (Debug, "Peers");
--        for Peer in Peers.Get_Peers_List.Iterate loop
--           System_Messages.Notify (Debug, To_String
--                           (Peer_List_Type.Element (Peer).Peer));
--           --           Peers.Print_Peer (Peer_List_Type.Element (Peer));
--        end loop;
--     end Test_Status_Print;

--     procedure Unpark (Call_Id : in     String;
--                       Status  :    out Status_Type) is
--        use Peers;
--        use Call_List;
--        use Peers.Call_List;

--        --        Peer_Cursor : Cursor;
--        Peer : Peers.Peer_Type;
--        Call : Call_Type;
--     begin
--        Status := Unknown_Error;

--        --        Peer := Peers.Get_Peer (To_Unbounded_String (Agent_ID));
--        --        if Peer = null_Peer then
--        --           Status := No_Agent_Found;
--        --           return;
--        --        end if;

--        --  Checks if the Peer is already in a call.
--        --        if Peer.Call /= Call_List.null_Call then
--        --           --  Something is wrong here.
--        --           System_Messages.Notify
--        --             (Info,
--        --              "There were a request for unPark a call with Agent: " &
--        --                Agent & " Call_ID: " & Call_ID &
--        --                " Call allready there: " &
--        --                To_String (Peer.Call.Channel));
--        --           Status := Agent_Already_In_Call;
--        --           return;
--        --        end if;

--        System_Messages.Notify (Debug,
--                        "Routines.Unpark:" &
--                          " Checkpoint before looking for the call");

--        --  then find the call.
--        --        Peer_Cursor := Peer.Parked_Calls.First;
--        --        Find_Parked_Call :
--        --        loop
--        --           exit Find_Parked_Call when
--        --             Peer_Cursor = Peers.Call_List.No_Element;
--        --
--        --           --  when found, move it to current call.
--        --           if Call_ID = Element (Peer_Cursor).Uniqueid then
--        --
--        --              --  Move call from parked calls, to Current_call
--        --              Peer.Call := Element (Peer_Cursor);
--        --              Delete (Container => Peer.Parked_Calls,
--        --                      Position  => Peer_Cursor);
--        --
--        --              --  Make the actual Asterisk call to transfer the call.
--        --              AMI.Action.Action_Manager.Redirect
--        --                (Channel  => To_String (Peer.Call.Channel),
--        --                 Context  => "LocalSets",
--        --                 Exten    => To_String (Peer.Exten));
--        --
--        --              --  Set the Variable CallState to empty,
--        --              --    so we can see that the call is not on hold any more
--        --              AMI.Action.Action_Manager.Set_Var
--        --                (Channel      => To_String (Peer.Call.Channel),
--        --                 VariableName => "CallState",
--        --                 Value        => "");
--        --
--        --              Status := Success;
--        --              return;
--        --           else
--        --              System_Messages.Notify (Debug,
--        --                              "Routines.Unpark: Call_ID /= Found id " &
--        --                                Call_ID & " /= " & To_String
--        --                                (Element (Peer_Cursor).Uniqueid));
--        --           end if;
--        --              Peer_Cursor := Next (Peer_Cursor);
--        --        end loop Find_Parked_Call;

--        Call := Call_List.Get_Call (To_Unbounded_String (Call_Id));

--        --  Checks if a call with that ID exsist
--        if Call = Null_Call then
--           Status := No_Call_Found;
--           return;
--        end if;

--        --  Makes sure that the call is not in a state,
--        --   where it would be inappropriate to change it's state.
--        if Call.State /= Speaking then
--           Status := Unknown_Error;
--           return;
--        end if;

--        Peer := Peers.Get_Peer_By_ID (Call.Agent_ID);

--        if Peer = Null_Peer then
--           Status := No_Agent_Found;
--           return;
--        end if;

--        AMI.Action.Redirect
--          (Channel  => To_String (Call.Channel),
--           Context  => "LocalSets",
--           Exten    => To_String (Peer.Exten));

--        System_Messages.Notify (Debug, "Routines.Unpark:" &
--                          " Checkpoint after looking for parkedcall");
--        --  If the call, for some unknown reason, wasn't there.
--        Status := No_Call_Found;
--     end Unpark;
end AMI.Action;
