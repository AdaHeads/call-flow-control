with Ada.Containers.Vectors;

with Call_Queue,
     Event_Parser;
package AMI.Action is
   use Event_Parser;

   package Call_List is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Call_Queue.Call_Type,
                             "="          => Call_Queue."=");

   task Action_Manager is
      --  Makes two channels talk with each other.
      entry Bridge (ChannelA : in String;
                    ChannelB : in String);

      --  Gives some information about the system
      entry CoreSettings (Data : out Event_List_Type.Map);

      --  Logs in this Action module
      entry Login (Username : in     String;
                   Secret   : in     String;
                   Success  :    out Boolean);

      --  Logs out this Action module.
      entry Logoff;

      --  Returns a list of all the calls waiting in a queue.
      entry QueueStatus (List : out Call_List.Vector);

      --  Sends a call on hold/park
      entry Park (Channel1 : in String;
                  Channel2 : in String);
      --  Ping pong
      entry Ping;

      --  Redirects a call, to another extensen in the dial plan.
      entry Redirect (Channel : in String;
                      Exten   : in String;
                      Context : in String);

      entry Initialize (Server_Host : in String;
                        Server_Port : in Positive;
                        Username    : in String;
                        Secret      : in String);
   end Action_Manager;

private
   --  Utility functions
   function Read_Event_List return Event_List_Type.Map;

      --  Actions
   procedure Bridge (ChannelA : in String;
                     ChannelB : in String);
   function CoreSettings return Event_List_Type.Map;
   function Login (Username : in String;
                   Secret   : in String) return Boolean;
   procedure Logoff;
   procedure Park (Channel          : in String;
                   Fallback_Channel : in String);
   procedure Ping;
   function QueueStatus (ActionID : in String := "") return Call_List.Vector;
   procedure Redirect (Channel : in String;
                       Exten   : in String;
                       Context : in String);
end AMI.Action;
