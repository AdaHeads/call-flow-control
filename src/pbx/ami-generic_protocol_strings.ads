-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              AMI.Protocol                                 --
--                                                                           --
--                                  SPEC                                     --
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

--  Protocol-specific strings and ... stuff
generic
   Asynchronous    : Boolean := True;
package AMI.Generic_Protocol_Strings is
   type Pause_States is (Pause, UnPause);

   function Bridge (Channel1  : in String;
                    Channel2  : in String;
                    Action_ID : in Action_ID_Type) return String with inline;

   function CoreSettings (Action_ID : in Action_ID_Type)
                          return String with inline;

   function Get_Var (Channel      : in String;
                     VariableName : in String;
                     Action_ID    : in Action_ID_Type)
                     return String with inline;

   function Hangup (Channel   : in String;
                    Action_ID : in Action_ID_Type) return String with inline;

   function Login (Username  : in String;
                   Secret    : in String;
                   Action_ID : in Action_ID_Type) return String with inline;

   function Logoff (Action_ID : in Action_ID_Type) return String with inline;

   function Park (Channel          : in String;
                  Fallback_Channel : in String;
                  Action_ID        : in Action_ID_Type)
                  return String with inline;

   function Ping (Action_ID : in Action_ID_Type) return String with inline;

   function QueuePause (DeviceName : in String;
                        State      : in Pause_States;
                        Action_ID  : in Action_ID_Type)
                        return String with inline;

   function QueueStatus (Action_ID : in Action_ID_Type)
                         return String with inline;

   function Redirect (Channel   : in String;
                      Context   : in String;
                      Exten     : in String;
                      Priority  : in Integer := 1;
                      Action_ID : in Action_ID_Type) return String with inline;

   function Set_Var (Channel       : in String;
                     VariableName  : in String;
                     Value         : in String;
                     Action_ID     : in Action_ID_Type)
                     return String with inline;

   function Next_Action_ID return Action_ID_Type;

private
   Current_Action_ID : Action_ID_Type := Action_ID_Type'First;
   pragma Atomic (Current_Action_ID);
end AMI.Generic_Protocol_Strings;
