-------------------------------------------------------------------------------
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

with
  Ada.Exceptions,
  Ada.Interrupts.Names;
with
  System_Messages;

package body SIGHUP is
   type Handler_Count is range 0 .. 20;
   subtype Handler_Indices is Handler_Count range 1 .. Handler_Count'Last;
   type Handler_States is array (Handler_Indices) of Boolean;

   protected Queue is
      procedure HUP;
      pragma Attach_Handler (HUP, Ada.Interrupts.Names.SIGHUP);

      procedure Stop;

      entry Wait (Handler_Indices) (Stop :    out Boolean);

      procedure Register (Handler_Index :    out Handler_Indices);
   private
      Handle     : Handler_States := (others => False);
      Registered : Handler_Count := 0;
      Stopping   : Boolean := False;
   end Queue;

   protected body Queue is
      procedure HUP is
      begin
         for H of Handle (1 .. Registered) loop
            H := True;
         end loop;
      end HUP;

      procedure Stop is
      begin
         Stopping := True;
      end Stop;

      entry Wait (for Handler in Handler_Indices) (Stop :    out Boolean)
        when Handle (Handler) is
      begin
         Handle (Handler) := False;
         Stop := Stopping;
      end Wait;

      procedure Register (Handler_Index :    out Handler_Indices) is
      begin
         if Registered < Handler_Indices'Last then
            Registered := Registered + 1;
            Handler_Index := Registered;
         else
            raise Constraint_Error;
         end if;
      end Register;
   end Queue;

   task type Watcher (Index : Handler_Indices;
                      Call  : Callback);
   type Watcher_Reference is access Watcher;

   task body Watcher is
      Stop : Boolean := False;
   begin
      System_Messages.Notify
        (Level   => System_Messages.Information,
         Message => "SIGHUP watcher " & Handler_Indices'Image (Index) &
                    " starting.");

      loop
         begin
            Queue.Wait (Index) (Stop);
            exit when Stop;
            Call.all;
         exception
            when E : others =>
               System_Messages.Notify
                 (Level   => System_Messages.Error,
                  Message => "SIGHUP handler " &
                             Handler_Indices'Image (Index) &
                             " raised the exception " &
                             Ada.Exceptions.Exception_Name (E) &
                             " with the message '" &
                             Ada.Exceptions.Exception_Message (E) & "'.");
         end;
      end loop;

      System_Messages.Notify
        (Level   => System_Messages.Information,
         Message => "SIGHUP watcher " & Handler_Indices'Image (Index) &
                    " shutting down.");
   end Watcher;

   procedure Register (Handler : in     Callback) is
      Index       : Handler_Indices;
      New_Watcher : Watcher_Reference;
   begin
      Queue.Register (Handler_Index => Index);
      New_Watcher := new Watcher (Index => Index,
                                  Call  => Handler);
   exception
      when Constraint_Error =>
         raise Constraint_Error
           with "Attempted to register too many handlers for SIGHUP.";
   end Register;

   procedure Stop is
   begin
      Queue.Stop;
   end Stop;
end SIGHUP;
