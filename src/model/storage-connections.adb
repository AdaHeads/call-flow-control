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
   --  TODO write comment

   procedure Generate
     (Count : in Positive;
      As    : in Connected_Mode);
   --  TODO write comment

   function Is_Active
     (Connection : in Instance)
      return Boolean;
   --  TODO write comment

   ------------------------
   --  Build_Connection  --
   ------------------------

   function Build_Connection
     (As : in Connected_Mode)
      return Instance is separate;

   ----------------
   --  Generate  --
   ----------------

   procedure Generate (Count : in Positive;
                       As    : in Connected_Mode) is
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
   begin
      return Result : Instance do
         for State in reverse As .. Connected_Mode'Last loop
            Working (State).Get (Item    => Result,
                                 Default => (State => Off_Line));
            exit when Result.State /= Off_Line;
         end loop;

         --  TODO check for empty failed list and log critical if empty

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
   --  TODO write comment

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
            delay 10.0; -- What is the optimal value here?  TODO!
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
      Failed.Enqueue (Connection);
      Connection := (State => Off_Line);
   end Queue_Failed;

   -----------------------------
   --  Stop_Maintenance_Task  --
   -----------------------------

   procedure Stop_Maintenance_Task
   is
   begin
      abort Maintenance;
   end Stop_Maintenance_Task;

begin

   Generate (Count => Config.Get (Max_Connection) + 1, As => Read_Only);
   Generate (Count => Config.Get (Max_Connection) + 1, As => Read_Write);

end Storage.Connections;
