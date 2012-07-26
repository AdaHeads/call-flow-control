--  Protocol-specific strings and ... stuff
package AMI.Protocol is
   function Bridge (Channel1 : in String;
                    Channel2 : in String) return String with inline;

   function CoreSettings return String with inline;

   function Login (Username : in String;
                   Secret : in String) return String with inline;

   function Logoff return String with inline;

   function Ping return String with inline;

   type Pause_States is (Pause, UnPause);

   function QueuePause (DeviceName : in String;
                        State : in Pause_States) return String with inline;

   function QueueStatus (ActionID : in String := "") return String with inline;

   function Redirect (Channel  : in String;
                      Context  : in String;
                      Exten    : in String;
                      Priority : in Integer := 1) return String with inline;

end AMI.Protocol;
