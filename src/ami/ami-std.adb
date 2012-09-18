-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 AMI.Std                                   --
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
with AMI.Action;
with AMI.Event;
with AWS.Net.Std;
with AWS.Net.Buffered;
with My_Configuration;
with Yolk.Log;

package body AMI.Std is

   Action_Socket : AWS.Net.Std.Socket_Type;
   Event_Socket  : AWS.Net.Std.Socket_Type;
   Shutdown      : Boolean := False;

   task AMI_Action_Task is
      entry Start;
      --  TODO: Write comment
   end AMI_Action_Task;

   task AMI_Event_Task is
      entry Start;
      --  TODO: Write comment.
   end AMI_Event_Task;

   -----------------------
   --  AMI_Action_Task  --
   -----------------------

   task body AMI_Action_Task
   is
      use My_Configuration;
      use Yolk.Log;

      Reconnect_Delay : constant Duration := 1.0;
   begin
      accept Start;

      Reconnect :
      loop
         begin
            AWS.Net.Std.Connect (Socket => Action_Socket,
                                 Host   => Config.Get (PBX_Host),
                                 Port   => Config.Get (PBX_Port));

            Trace (Info,
                   "AMI action socket connected - Host: "
                   & Config.Get (PBX_Host)
                   & " Port: " & Config.Get (PBX_Port));

            AMI.Action.Start (Socket   => Action_Socket,
                              Username => Config.Get (PBX_Action_User),
                              Secret   => Config.Get (PBX_Action_Secret));
            Trace (Debug, "AMI action returned out of start");
         exception
            when Err : others =>
               Trace (Info,
                      "ami-std, AMI Action, " &
                        "ExceptionName: " &
                        Ada.Exceptions.Exception_Name (Err));
         end;

         if Shutdown then
            Trace (Info, "AMI action connection Closed");
            exit Reconnect;
         end if;

         delay Reconnect_Delay;
      end loop Reconnect;
   end AMI_Action_Task;

   ----------------------
   --  AMI_Event_Task  --
   ----------------------

   task body AMI_Event_Task
   is
      use Ada.Exceptions;
      use My_Configuration;
      use Yolk.Log;

      Reconnect_Delay : constant Duration := 1.0;
   begin
      accept Start;

      Reconnect :
      loop
         begin
            AWS.Net.Std.Connect (Socket => Event_Socket,
                                 Host   => Config.Get (PBX_Host),
                                 Port   => Config.Get (PBX_Port));

            Trace (Info,
                   "AMI event socket connected - Host: "
                   & Config.Get (PBX_Host)
                   & " Port: " & Config.Get (PBX_Port));

            AMI.Event.Start (Channel  => Event_Socket,
                             Username => Config.Get (PBX_Event_User),
                             Secret   => Config.Get (PBX_Event_Secret));
         exception
            when Err : others =>
               Trace (Info,
                      "ami-std, AMI Event, " &
                        "ExceptionName: " &
                        Ada.Exceptions.Exception_Name (Err));

         end;

         if Shutdown then
            Trace (Info, "AMI socket connection closed.");
            exit Reconnect;
         else
            --  send message out to websocket about system failure.
            Trace (Error, "No connection to AMI.");
         end if;

         delay Reconnect_Delay;
      end loop Reconnect;
   exception
      when Err : others =>
         Trace
           (Debug,
            "Exception in AMI-STD.AMI_Service:" &
              Exception_Name (Err) & "|:|" & Exception_Message (Err));
   end AMI_Event_Task;

   ---------------
   --  Connect  --
   ---------------

   procedure Connect
   is
      use My_Configuration;
      use Yolk.Log;
   begin
      AMI_Event_Task.Start;

      Trace (Info, "AMI event socket initialized.");

      AMI_Action_Task.Start;

      Trace (Info, "AMI action socket initialized.");

      --  ???? Should we block here until the sockets are actually connected?
      --  If we don't block, we run the risk of clients connecting to Alice
      --  while she's in a state where we're not actually able to  communicate
      --  with the AMI.
   end Connect;

   ------------------
   --  Disconnect  --
   ------------------

   procedure Disconnect
   is
   begin
      Shutdown := True;
      AWS.Net.Buffered.Shutdown (Event_Socket);
   end Disconnect;

end AMI.Std;
