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
with PBX.Trace;

package body PBX.Call is

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
   begin
      Call_List.Change_State (Obj.ID, New_State);
   end Change_State;

   ------------------------
   -- Create_And_Insert  --
   ------------------------

   procedure Create_And_Insert
     (Inbound         : in Boolean;
      ID              : in Identification;
      State           : in States := Unknown;
      Extension       : in String := "";
      From_Extension  : in String := "")
   is
      Call : constant Instance :=
               (ID             => ID,
                Inbound        => Inbound,
                State          => State,
                Organization_ID => 1,
                Assigned_To     => 1,
                Extension      => To_Unbounded_String (Extension),
                From_Extension => To_Unbounded_String (From_Extension),
                Arrived        => Current_Time,
                B_Leg          => Null_Identification);
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
      if Call = Null_Identification then
         return Null_Instance;
      end if;

      return Call_List.Get (Call);
   end Get;

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
   begin
      Call_List.Link (ID_1, ID_2);
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

   -----------------------
   --  Organization_ID  --
   -----------------------

   function Organization_ID (Obj : in Instance) return Natural is
   begin
      return Obj.Organization_ID;
   end Organization_ID;

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

   --------------
   --  Remove  --
   --------------

   function Remove (ID : in Identification) return Instance is
      Context : constant String := Package_Name & ".Remove";
      Removed_Call : Instance := Null_Instance;
   begin
      PBX.Trace.Debug (Message => "Removing call with ID " & To_String (ID),
                       Context => Context,
                       Level   => 1);

      --  Make a copy of the call to be able to return it.
      Removed_Call := Call_List.Get (ID);

      Call_List.Remove (ID);
      return Removed_Call;
   end Remove;

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
            PBX.Trace.Error ("Tried to decrement number of " &
                 "Queued calls below 0");
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

         PBX.Trace.Debug (Message => "Looking up call " & Image (ID),
                          Context => "PBX.Call.Get");
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
      exception
         when Constraint_Error =>
            raise Constraint_Error with "ID" & To_String (Item.ID);
      end Insert;

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

         List.Delete (ID);
      exception
         when Constraint_Error =>
            raise Not_Found with " call " & To_String (ID);
      end Remove;

      ---------------
      --  To_JSON  --
      ---------------

      function To_JSON (Only_Queued : Boolean := False)
                        return GNATCOLL.JSON.JSON_Value is
         use GNATCOLL.JSON;
         Value     : JSON_Value := Create_Object;
         JSON_List : JSON_Array;
         Root      : constant JSON_Value := Create_Object;
      begin
         for Call of List loop
            if not Only_Queued then
               Value := Call.To_JSON;
               Append (JSON_List, Value);
            elsif Only_Queued and then Call.State = Queued then
               Value := Call.To_JSON;
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
end PBX.Call;
