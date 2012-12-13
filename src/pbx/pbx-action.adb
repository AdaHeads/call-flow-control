with AMI.Response;

package body PBX.Action is

   function Cast ( Handler : Response_Handler)
                  return AMI.Packet.Action.Response_Handler_Type;
   function Cast (ID : AMI.Action_ID_Type)
                  return Reply_Ticket;

   ------------
   -- Hangup --
   ------------

   function Hangup (ID : in Call_ID.Call_ID_Type) return Reply_Ticket is
      use Call_ID;

      Hangup_Action : AMI.Packet.Action.Request :=
                        AMI.Packet.Action.Hangup
                          (Channel => ID.To_String);
   begin
      if ID /= Call_ID.Null_Call_ID then
         PBX.Client.Send (Hangup_Action.To_AMI_Packet);

         return Cast (Hangup_Action.Action_ID);
      end if;

      return Null_Reply;

   end Hangup;

   -------------------
   -- List_Channels --
   -------------------

   function List_Channels (On_Response : in Response_Handler :=
                             Ignore) return Reply_Ticket
   is
      List_Channels_Action  : AMI.Packet.Action.Request :=
                                AMI.Packet.Action.Core_Show_Channels
                                  (On_Response => Cast (On_Response));
   begin
      Client.Send (List_Channels_Action);

      return Cast (List_Channels_Action.Action_ID);
   end List_Channels;

   function Login (Username    : in String;
                   Secret      : in String;
                   On_Response : in Response_Handler :=
                     Ignore) return Reply_Ticket is
      Login_Action : AMI.Packet.Action.Request :=
                       AMI.Packet.Action.Login
                         (Username    => Username,
                          Secret      => Secret,
                          On_Response => Cast (On_Response));
   begin
      Client.Send (Login_Action);
      return Cast (Login_Action.Action_ID);
   end Login;

   ---------------------
   --  List_SIP_Peers --
   ---------------------

   function List_SIP_Peers (On_Response : in Response_Handler :=
                               Ignore) return Reply_Ticket is
      List_Peers_Action : AMI.Packet.Action.Request :=
                            AMI.Packet.Action.SIP_Peers
                              (On_Response => Cast (On_Response));
   begin
      Client.Send (List_Peers_Action);

      return Cast (List_Peers_Action.Action_ID);
   end List_SIP_Peers;

   ------------
   -- Logoff --
   ------------

   function Logoff (On_Response : in Response_Handler :=
                      Ignore) return Reply_Ticket is
      List_Peers_Action : AMI.Packet.Action.Request :=
                            AMI.Packet.Action.Logoff
                              (On_Response => Cast (On_Response));
   begin
      Client.Send (List_Peers_Action);

      return Cast (List_Peers_Action.Action_ID);
   end Logoff;


   --------------
   -- Wait_For --
   --------------

   procedure Wait_For (Ticket : in Reply_Ticket) is
   begin
      AMI.Response.Wait_For (Action_ID => Action_ID_Type (Ticket));
   end Wait_For;

   --------------------
   -- Cast functions --
   --------------------

   function Cast (Handler : Response_Handler)
                  return AMI.Packet.Action.Response_Handler_Type is
   begin
      return AMI.Packet.Action.Response_Handler_Type (Handler);
   end Cast;

   function Cast (ID : AMI.Action_ID_Type)
                  return Reply_Ticket is
   begin
      return Reply_Ticket (ID);
   end Cast;

end PBX.Action;
