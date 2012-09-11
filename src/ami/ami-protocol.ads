--  Protocol-specific strings and ... stuff
package AMI.Protocol is
   function Bridge (Channel1 : in String;
                    Channel2 : in String;
                    Async    : in Boolean := True) return String with inline;

   function CoreSettings (Async : in Boolean := True)
                          return String with inline;

   function Get_Var (Channel      : in String;
                     VariableName : in String;
                     Async        : in Boolean := True)
                     return String with inline;

   function Hangup (Channel : in String;
                    Async    : in Boolean := True) return String with inline;

   function Login (Username : in String;
                   Secret   : in String;
                   Async    : in Boolean := False) return String with inline;

   function Logoff (Async : in Boolean := True) return String with inline;

   function Park (Channel          : in String;
                  Fallback_Channel : in String;
                  Async            : in Boolean := True)
                  return String with inline;

   function Ping (Async : in Boolean := True) return String with inline;

   type Pause_States is (Pause, UnPause);

   function QueuePause (DeviceName : in String;
                        State      : in Pause_States;
                        Async      : in Boolean := True)
                        return String with inline;

   function QueueStatus (ActionID : in String := "";
                         Async    : in Boolean := True)
                         return String with inline;

   function Redirect (Channel  : in String;
                      Context  : in String;
                      Exten    : in String;
                      Priority : in Integer := 1;
                      Async    : in Boolean := True) return String with inline;

   function Set_Var (Channel      : in String;
                     VariableName : in String;
                     Value        : in String;
                     Async        : in Boolean := True)
                     return String with inline;
end AMI.Protocol;
