-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2013-, AdaHeads K/S                     --
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

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

with Non_Blocking_Stack;
with System_Message.Critical;
with System_Message.Info;

with Yolk.Configuration;

package body Storage.Connections is

   use Ada.Containers;
   use Yolk.Configuration;

   package Connection_Queue_Interface is new Synchronized_Queue_Interfaces
     (Element_Type => Instance);

   package Connection_Queues is new Unbounded_Synchronized_Queues
     (Queue_Interfaces => Connection_Queue_Interface);

   package Connection_Stacks is new Non_Blocking_Stack
     (Element_Type => Instance);

   Working : array (Connected_Mode) of Connection_Stacks.Instance;
   Failed  : Connection_Queues.Queue;

   function Build_Connection
     (As : in Connected_Mode)
      return Instance;
   --  Build the GNATCOLL Database_Connection.

   procedure Generate
     (Count : in Positive;
      As    : in Connected_Mode);
   --  Generate Count connections to the database according to the As
   --  Connected_Mode.

   function Is_Active
     (Connection : in Instance)
      return Boolean;
   --  Return True of the Connection is connected to the database.

   ------------------------
   --  Build_Connection  --
   ------------------------

   function Build_Connection
     (As : in Connected_Mode)
      return Instance is separate;

   ----------------
   --  Generate  --
   ----------------

   procedure Generate
     (Count : in Positive;
      As    : in Connected_Mode)
   is
      Connection : Instance;
   begin
      for I in 1 .. Count loop
         Connection := Build_Connection (As);
         if Is_Active (Connection) then
            Working (As).Push (Connection);
         else
            Failed.Enqueue (Connection);
         end if;
      end loop;
   end Generate;

   -----------
   --  Get  --
   -----------

   function Get
     (As : in Connected_Mode)
      return Instance
   is
      use System_Message;
   begin
      return Result : Instance do
         for Mode in Connected_Mode loop
            if Working (Mode).Is_Empty then
               Critical.Get_Storage_Connection_Error
                 (Message => Connected_Mode'Image (Mode));
            end if;
         end loop;

         for State in reverse As .. Connected_Mode'Last loop
            Working (State).Get (Item    => Result,
                                 Default => Off_Line_Instance);
            exit when Result.State /= Off_Line;
         end loop;
      end return;
   end Get;

   -----------------
   --  Is_Active  --
   -----------------

   function Is_Active
     (Connection : in Instance)
      return Boolean
   is
   begin
      return GNATCOLL.SQL.Exec.Check_Connection (Connection.Connection);
   end Is_Active;

   task Maintenance;
   --  Repeatedly check the Failed queue and try to reconnect connections.
   --  When a connection is established, move the connection from the Failed
   --  queue to the Working queue.

   -------------------
   --  Maintenance  --
   -------------------

   task body Maintenance is
      Connection : Instance;
   begin
      loop
         Failed.Dequeue (Connection);

         Connection := Build_Connection (Connection.State);

         if Is_Active (Connection) then
            Working (Connection.State).Push (Connection);
         else
            Failed.Enqueue (Connection);
            delay 5.0;
         end if;
      end loop;
   exception
      when E : others =>
         System_Message.Critical.Connection_Maintenance_Error (Event => E);
   end Maintenance;

   --------------------
   --  Queue_Failed  --
   --------------------

   procedure Queue_Failed
     (Connection : in out Instance)
   is
   begin
      if Connection.State /= Off_Line then
         Failed.Enqueue (Connection);

         --  If we still have some connetions in the Working queue, then fail
         --  those as well. It stands to reason that if one Connection.State
         --  connection doesn't work, then the rest are dead also. We do not
         --  want to hand out these dead connections to AWS tasks.
         while not Working (Connection.State).Is_Empty loop
            Failed.Enqueue (Get (As => Connection.State));
         end loop;
      end if;

      Connection := Off_Line_Instance;
   end Queue_Failed;

   -----------------------------
   --  Stop_Maintenance_Task  --
   -----------------------------

   procedure Stop_Maintenance_Task
   is
      use System_Message;
   begin
      abort Maintenance;
      Info.Database_Connection_Manager_Stop;
   end Stop_Maintenance_Task;

begin

   Generate (Count => Config.Get (Max_Connection), As => Read_Only);
   Generate (Count => Config.Get (Max_Connection), As => Read_Write);

end Storage.Connections;
