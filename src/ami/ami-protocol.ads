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
package AMI.Protocol is
   function Bridge (Channel1 : in String;
                    Channel2 : in String;
                    Async    : in Boolean := True) return String with inline;

   function CoreSettings (Async : in Boolean := True)
                          return String with inline;

   function Get_Var (Channel      : in String;
                     VariableName : in String;
                     ActionID     : in String := "";
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
