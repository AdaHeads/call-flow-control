with --  Ada.Containers.Vectors,
     Ada.Strings.Unbounded;

with --  Call_List,
     Event_Parser;

--  This package have the purpose of sending Actions/Commands to Asterisk,
--  and read the response
package AMI.Action is
   use Event_Parser;

--     package Call_List is new
--       Ada.Containers.Vectors (Index_Type   => Positive,
--                               Element_Type => Call_List.Call_Type,
--                               "="          => Call_List."=");

--     task Action_Manager is
--        --  Makes two channels talk with each other.
--        entry Bridge (ChannelA : in String;
--                      ChannelB : in String);
--
--        --  Gives some information about the system
--        entry CoreSettings (Data : out Event_List_Type.Map);
--
--        --  Returns a variable by the name, from a channel.
--        entry Get_Var
--          (Channel      : in String;
--           VariableName : in String;
--           Value        : out Ada.Strings.Unbounded.Unbounded_String);
--
--        entry Hangup (Channel : in String);
--
--        --  Logs in this Action module
--        entry Login (Username : in     String;
--                     Secret   : in     String;
--                     Success  :    out Boolean);
--
--        --  Logs out this Action module.
--        entry Logoff;
--
--        --  Returns a list of all the calls waiting in a queue.
--        entry QueueStatus;
--
--        --  Sends a call on hold/park
--        entry Park (Channel1 : in String;
--                    Channel2 : in String);
--
--        --  Ping pong
--        entry Ping;
--
--        --  Redirects a call, to another extensen in the dial plan.
--        entry Redirect (Channel : in String;
--                        Exten   : in String;
--                        Context : in String);
--
--        entry Initialize (Server_Host : in String;
--                          Server_Port : in Positive;
--                          Username    : in String;
--                          Secret      : in String);
--
--        entry Set_Var (Channel      : in String;
--                       VariableName : in String;
--                       Value        : in String);
--     end Action_Manager;

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

   procedure Login (Username : in String;
                    Secret   : in String);
end AMI.Action;
