-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               AMI.Action                                  --
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
