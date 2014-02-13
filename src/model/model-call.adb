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

with Ada.Strings;
with Ada.Strings.Fixed;
with View.Call;
with Handlers.Notifications;
with Client_Notification.Call,
     Client_Notification.Queue;
with System_Messages;

package body Model.Call is

   package Notification renames Handlers.Notifications;

   function "=" (Left, Right : in Identification) return Boolean is
   begin
      return ESL.UUID."=" (Left, Right);
   end "=";

   --------------------
   --  Arrival_Time  --
   --------------------

   function Arrival_Time (Obj : in Instance) return Common.Time is
   begin
      return Obj.Arrived;
   end Arrival_Time;

   -------------------
   --  Assigned_To  --
   -------------------

   function Assigned_To (Obj : in Instance) return Natural is
   begin
      return Obj.Assigned_To;
   end Assigned_To;

   -------------
   --  B_Leg  --
   -------------

   function B_Leg (Obj : in Instance) return Identification is
   begin
      return Obj.B_Leg;
   end B_Leg;

   --------------------
   --  Change_State  --
   --------------------

   procedure Change_State (Obj : in Instance; New_State : in States) is
      Context    : constant String := Package_Name & ".Change_State";
      Last_State : constant States := Get (Obj.ID).State;
   begin

      Call_List.Change_State (Obj.ID, New_State);
      case New_State is
         when Parked =>

            System_Messages.Debug
              (Message => "Parked call: " & Get (Obj.ID).To_JSON.Write,
               Context => Context);

            Notification.Broadcast
              (Client_Notification.Call.Park
                 (C => Get (Obj.ID)).To_JSON);

         when Ringing =>
            System_Messages.Debug
              (Message =>
               "New call arrived: " & Get (Obj.ID).To_JSON.Write,
               Context => Context);

            Notification.Broadcast
              (Client_Notification.Call.Offer_Call (Get (Obj.ID)).To_JSON);
         when Queued =>
            System_Messages.Debug
              ("Call queued: " & Get (Obj.ID).To_JSON.Write,
               Context => Context);

            Notification.Broadcast
              (Client_Notification.Queue.Join
                 (Get (Call => Obj.ID)).To_JSON);

         when Hungup =>
            --  Assert that the call leaves a queue or a parking lot.
            if Last_State = Queued then
               Call_List.Get (Obj.ID).Change_State (New_State => Left_Queue);
            elsif Last_State = Parked then
               Call_List.Get (Obj.ID).Change_State (New_State => Unparked);
            end if;

            System_Messages.Debug
              ("Call hung up: " & Get (Obj.ID).To_JSON.Write,
               Context => Context);

            Call_List.Remove (ID => Obj.ID);

         when Left_Queue =>
            System_Messages.Debug
              ("Call left queue: " & Get (Obj.ID).To_JSON.Write,
               Context => Context);

            Notification.Broadcast
              (Client_Notification.Queue.Leave (Get (Obj.ID)).To_JSON);
         when others =>
            null;
      end case;

   end Change_State;

   ------------------------
   -- Create_And_Insert  --
   ------------------------

   procedure Create_And_Insert
     (Inbound         : in Boolean;
      ID              : in Identification;
      Reception_ID    : in Reception_Identifier;
      Extension       : in String := "";
      From_Extension  : in String := "")

   is
      Call : constant Instance :=
               (ID              => ID,
                Inbound         => Inbound,
                State           => Unknown,
                Reception_ID    => Reception_ID,
                Greeting_Played => <>,
                Locked          => <>,
                Assigned_To     => 1,
                Extension       => To_Unbounded_String (Extension),
                From_Extension  => To_Unbounded_String (From_Extension),
                Arrived         => Current_Time,
                B_Leg           => Null_Identification);
   begin
      Call_List.Insert (Item => Call);
   end Create_And_Insert;

   function Extension (Obj : in Instance) return String is
   begin
      return To_String (Obj.Extension);
   end Extension;

   function From_Extension (Obj : in Instance) return String is
   begin
      return To_String (Obj.From_Extension);
   end From_Extension;

   -----------
   --  Get  --
   -----------

   function Get (Call : Identification) return Instance is
   begin
      if not Call_List.Contains (Call) then
         raise Not_Found;
      end if;

      return Call_List.Get (Call);
   end Get;

   function Greeting_Played (Obj : in Instance) return Boolean is
   begin
      return Obj.Greeting_Played;
   end Greeting_Played;

   function Has (ID : Identification) return Boolean is
   begin
      return Call_List.Contains (ID);
   end Has;

   -------------------------
   --  Highest_Prioirity  --
   -------------------------
   function Highest_Prioirity return Instance is
   begin
      return Call_List.First;
   end Highest_Prioirity;

   ----------
   --  ID  --
   ----------

   function ID (Obj : in Instance) return Identification is
   begin
      return Obj.ID;
   end ID;

   ---------------
   --  Inbound  --
   ---------------

   function Inbound (Obj : in Instance) return Boolean is
   begin
      return Obj.Inbound;
   end Inbound;

   procedure Link (ID_1 : in Identification;
                   ID_2 : in Identification) is
      Context : constant String := Package_Name & ".Link";
   begin
      Call_List.Link (ID_1, ID_2);

      System_Messages.Debug
        (Message => Client_Notification.Call.Pickup
           (Get (ID_1)).To_JSON.Write,
         Context => Context);

      Notification.Broadcast
        (Client_Notification.Call.Pickup (Get (ID_1)).To_JSON);

   end Link;

   ------------
   --  List  --
   ------------

   function List return GNATCOLL.JSON.JSON_Value is
   begin
      return Call_List.To_JSON;
   end List;

   ------------------
   --  List_Empty  --
   ------------------

   function List_Empty return Boolean is
   begin
      return Call_List.Empty;
   end List_Empty;

   procedure Lock (Obj : in Instance) is
   begin
      Call_List.Set_Locked (ID     => Obj.ID,
                            Locked => True);
   end Lock;

   ---------------------------
   --  Null_Identification  --
   ---------------------------

   function Null_Identification return Identification is
   begin
      return ESL.UUID.Null_UUID;
   end Null_Identification;

   ---------------------
   --  Null_Instance  --
   ---------------------

   function Null_Instance return Instance is
   begin
      return (ID              => Null_Identification,
              State           => States'First,
              Inbound         => False,
              Locked          => True,
              Greeting_Played => <>,
              Reception_ID    => <>,
              Assigned_To     => <>,
              Extension       => Null_Unbounded_String,
              From_Extension  => Null_Unbounded_String,
              B_Leg           => Null_Identification,
              Arrived         => Common.Null_Time);
   end Null_Instance;

   -------------------
   --  Queue_Count  --
   -------------------

   function Queue_Count return Natural is
   begin
      return Call_List.Queued;
   end Queue_Count;

   -------------------
   --  Queue_Empty  --
   -------------------

   function Queue_Empty return Boolean is
   begin
      if Call_List.Empty then
         return True;
      else
         return Call_List.Queued > 0;
      end if;
   end Queue_Empty;

   --------------------
   --  Queued_Calls  --
   --------------------

   function Queued_Calls return GNATCOLL.JSON.JSON_Value is
   begin
      return Call_List.To_JSON (Only_Queued => True);
   end Queued_Calls;

   --------------------
   --  Reception_ID  --
   --------------------

   function Reception_ID (Obj : in Instance) return
     Reception_Identifier is
   begin
      return Obj.Reception_ID;
   end Reception_ID;

   -------------
   --  State  --
   -------------

   function State (Obj : in Instance) return States is
   begin
      return Obj.State;
   end State;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON (Obj : in Instance) return GNATCOLL.JSON.JSON_Value is
   begin
      return View.Call.To_JSON (Obj);
   end To_JSON;

   -----------------
   --  To_String  --
   -----------------

   function To_String (Item : in Identification) return String is
      use Ada.Strings;

      Value : String renames Item.Image;
   begin
      return Ada.Strings.Fixed.Trim (Source => Value,
                                     Side   => Left);
   end To_String;

   --------------
   --  Unlink  --
   --------------

   procedure Unlink (ID : in Identification) is
   begin
      Call_List.Unlink (ID);
   end Unlink;

   --------------
   --  Unlock  --
   --------------

   procedure Unlock (Obj : in Instance) is
   begin
      Call_List.Set_Locked (ID     => Obj.ID,
                            Locked => False);
   end Unlock;

   -------------
   --  Value  --
   -------------

   function Value (Item : in String) return Identification is
   begin
      return ESL.UUID.Create (Item => Item);
   exception
      when Constraint_Error =>
         raise Invalid_ID with Item;
   end Value;

   -----------------
   --  Call_List  --
   -----------------

   protected body Call_List is

      --------------------
      --  Change_State  --
      --------------------

      procedure Change_State (ID        : in Identification;
                              New_State : in States) is
         procedure Update (Key     : in     Identification;
                           Element : in out Instance);

         procedure Update (Key     : in     Identification;
                           Element : in out Instance) is
            pragma Unreferenced (Key);
         begin
            Element.State := New_State;

            if New_State = Queued then
               Element.Greeting_Played := True;
            end if;

         end Update;
      begin
         Call_List.Update (ID, Update'Access);
      end Change_State;

      ----------------
      --  Contains  --
      ----------------

      function Contains (ID : in Identification)
                         return Boolean is
      begin
         return List.Contains (ID);
      end Contains;

      ---------------
      --  Dequeue  --
      ---------------

      procedure Dequeue (ID : in Identification) is

         Context : constant String := Package_Name & ".Call_List.Dequeue";

         procedure Update (Key     : in     Identification;
                           Element : in out Instance);

         procedure Update (Key     : in     Identification;
                           Element : in out Instance) is
            pragma Unreferenced (Key);
         begin
            Element.State := Transferring;
         end Update;
      begin
         Call_List.Update (ID, Update'Access);
         if Number_Queued = 0 then
            System_Messages.Error
              ("Tried to decrement number of " &
                 "Queued calls below 0",
              Context => Context);
         else
            Number_Queued := Number_Queued - 1;
         end if;
      end Dequeue;

      -------------
      --  Empty  --
      -------------

      function Empty return Boolean is
      begin
         return List.Is_Empty;
      end Empty;

      ---------------
      --  Enqueue  --
      ---------------

      procedure Enqueue (ID : in Identification) is
         procedure Update (Key     : in     Identification;
                           Element : in out Instance);

         procedure Update (Key     : in     Identification;
                           Element : in out Instance) is
            pragma Unreferenced (Key);
         begin
            Element.State := Queued;
         end Update;
      begin
         Call_List.Update (ID, Update'Access);
         Number_Queued := Number_Queued + 1;
      end Enqueue;

      -------------
      --  First  --
      -------------

      function First return Instance is
      begin
         return List.First_Element;
      end First;

      -----------
      --  Get  --
      -----------

      function Get (ID : in Identification) return Instance is
      begin
         if not List.Contains (ID) then
            raise Not_Found;
         else
            return List.Element (ID);
         end if;
      end Get;

      --------------
      --  Insert  --
      --------------

      procedure Insert (Item : Instance) is
         --  Agent : Model.Agent.Agent_Type := Model.Agent.Null_Agent;
      begin
--           if
--             Item.Assigned_To = Null_Agent_ID and
--           then
--              raise Constraint_Error with
--                "Both agent ID and channel cannot be null";
--           end if;
         List.Insert (Key      => Item.ID,
                      New_Item => Item);
         System_Messages.Debug
           (Message => "Inserted call" & Item.ID.Image,
            Context => Package_Name & "Call_List.Insert");
         pragma Assert (List.Contains (Key => Item.ID));
      exception
         when Constraint_Error =>
            raise Constraint_Error with "ID" & To_String (Item.ID);
      end Insert;

      ------------
      --  Link  --
      ------------

      procedure Link (ID_1, ID_2 : in Identification) is
         procedure Update (Key     : in     Identification;
                           Element : in out Instance);

         procedure Update (Key     : in     Identification;
                           Element : in out Instance) is
         begin
            if Key = ID_1 then
               Element.B_Leg := ID_2;
            else
               Element.B_Leg := ID_1;
            end if;
         end Update;
      begin
         Call_List.Update (ID_1, Update'Access);
         Call_List.Update (ID_2, Update'Access);
      end Link;

      --------------
      --  Queued  --
      --------------

      function Queued return Natural is
      begin
         return Number_Queued;
      end Queued;

      --------------
      --  Remove  --
      --------------

      procedure Remove (ID : in Identification) is
      begin
         List.Delete (Key => ID);
      exception
         when E : Constraint_Error =>
            raise Not_Found with " call " & To_String (ID);
      end Remove;

      ------------------
      --  Set_Locked  --
      ------------------

      procedure Set_Locked (ID     : in Identification;
                            Locked : in Boolean) is
         procedure Set_Lock (Key     : in     Identification;
                           Element : in out Instance);

         procedure Set_Lock (Key     : in     Identification;
                             Element : in out Instance) is
            pragma Unreferenced (Key);
         begin
            Element.Locked := Locked;
         end Set_Lock;
      begin
         Call_List.Update (ID, Set_Lock'Access);
      end Set_Locked;

      ---------------
      --  To_JSON  --
      ---------------

      function To_JSON (Only_Queued : Boolean := False)
                        return GNATCOLL.JSON.JSON_Value is
         use GNATCOLL.JSON;
         use Call_Storage;
         Value     : JSON_Value := Create_Object;
         JSON_List : JSON_Array;
         Root      : constant JSON_Value := Create_Object;
      begin
         for C in List.Iterate loop
            if not Only_Queued then
               Value := Element (C).To_JSON;
               Append (JSON_List, Value);
            elsif Only_Queued and then Element (C).State = Queued then
               Value := Element (C).To_JSON;
               Append (JSON_List, Value);
            end if;
         end loop;

         Root.Set_Field ("calls", JSON_List);
         return Root;
      end To_JSON;

      --------------
      --  Unlink  --
      --------------

      procedure Unlink (ID : in Identification) is
         procedure Update (Key     : in     Identification;
                           Element : in out Instance);

         procedure Update (Key     : in     Identification;
                           Element : in out Instance) is
            pragma Unreferenced (Key);
         begin
            Element.B_Leg := Null_Identification;
         end Update;
      begin
         --  TODO: Update B-leg.
         Call_List.Update (ID, Update'Access);
      end Unlink;

      --------------
      --  Update  --
      --------------

      procedure Update (ID    : in Identification;
                        Process : not null access procedure
                          (Key     : in     Identification;
                           Element : in out Instance)) is
      begin
         List.Update_Element (Position => List.Find (ID),
                              Process  => Process);
      end Update;
   end Call_List;
end Model.Call;
