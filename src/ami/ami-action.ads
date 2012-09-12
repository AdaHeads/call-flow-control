with AWS.Net.Std;
with Event_Parser;

--  This package have the purpose of sending Actions/Commands to Asterisk,
--  and read the response
package AMI.Action is
   use Event_Parser;

   procedure Start (Socket   : in AWS.Net.Std.Socket_Type;
                    Username : in String;
                    Secret   : in String);

   --  Actions
   procedure Bridge (ChannelA : in String;
                     ChannelB : in String);
--     function  CoreSettings return Event_List_Type.Map;
   procedure  Get_Var (Channel      : in String;
                       VariableName : in String);

   procedure Hangup (Channel : String);

   procedure Logoff;
   procedure Park (Channel          : in String;
                   Fallback_Channel : in String);
   procedure Ping;
--     function  QueueStatus (ActionID : in String := "")
--                            return Call_List.Call_List_Type.Vector;
   procedure QueueStatus (ActionID : in String := "");
   procedure Redirect (Channel : in String;
                       Exten   : in String;
                       Context : in String);
   procedure Set_Var (Channel      : in String;
                      VariableName : in String;
                      Value        : in String);

private
   --  Utility functions
   function Read_Event_List return Event_List_Type.Map;

   function Login (Username : in String;
                   Secret   : in String) return Boolean;
end AMI.Action;
