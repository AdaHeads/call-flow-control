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

package body PBX.Call is

   --------------------
   --  Arrival_Time  --
   --------------------

   function Arrival_Time (Obj : in Instance) return Common.Time is
   begin
      return Obj.Arrived;
   end Arrival_Time;

   --------------
   --  Assign  --
   --------------

   procedure Assign (Obj : in Instance; To : in Agent_ID_Type) is
   begin
      null;
   end Assign;

   -------------------
   --  Assigned_To  --
   -------------------

   function Assigned_To (Obj : in Instance)
                         return Model.Agent_ID.Agent_ID_Type is
   begin
      return Obj.Assigned_To;
   end Assigned_To;

   -------------
   --  B_Leg  --
   -------------

   function B_Leg (Obj : in Instance) return Channel_Identification is
   begin
      return Obj.B_Leg;
   end B_Leg;

   ---------------
   --  Channel  --
   ---------------

   function Channel (Obj : in Instance) return Channel_Identification is
   begin
      return Obj.Channel;
   end Channel;

   ------------------------
   -- Create_And_Insert  --
   ------------------------

   function Create_And_Insert
     (Inbound         : in Boolean;
      Channel         : in String;
      State           : in States := Unknown;
      Organization_ID : in Natural := 0;
      B_Leg           : in String := "")
      return Instance
   is
      Call : constant Instance :=
               (ID             => Next,
                Inbound        => Inbound,
                Channel        => Channel_Identification
                  (Ada.Strings.Unbounded.To_Unbounded_String (Channel)),
                State          => State,
                Organization   =>
                  Model.Organization_Identifier (Organization_ID),
                Arrived        => Current_Time,
                B_Leg          => Channel_Identification
                  (Ada.Strings.Unbounded.To_Unbounded_String (B_Leg)),
                Assigned_To    => Null_Agent_ID);
   begin
      Call_List.Insert (Item => Call);
      return Call;
   end Create_And_Insert;

   ------------------------
   -- Create_And_Insert  --
   ------------------------

   procedure Create_And_Insert
     (Inbound         : in Boolean;
      Channel         : in String;
      State           : in States := Unknown;
      Organization_ID : in Natural := 0;
      B_Leg           : in String := "")
   is
      Call : constant Instance :=
               (ID             => Next,
                Inbound        => Inbound,
                Channel        => Channel_Identification
                  (Ada.Strings.Unbounded.To_Unbounded_String (Channel)),
                State          => State,
                Organization   =>
                  Model.Organization_Identifier (Organization_ID),
                Arrived        => Current_Time,
                B_Leg          => Channel_Identification
                  (Ada.Strings.Unbounded.To_Unbounded_String (B_Leg)),
                Assigned_To    => Null_Agent_ID);
   begin
      Call_List.Insert (Item => Call);
   end Create_And_Insert;

   ---------------
   --  Dequeue  --
   ---------------

   procedure Dequeue (Obj : in Instance) is
   begin
      Call_List.Dequeue (Obj.ID);
   end Dequeue;

   ------------
   --  Dial  --
   ------------

   procedure Dial
     (Obj : in Instance; Destination : in Channel_Identification) is

      procedure Update (Key     : in     Identification;
                        Element : in out Instance);

      procedure Update (Key     : in     Identification;
                        Element : in out Instance) is
         pragma Unreferenced (Key);
      begin
         Element.State := Dialing;
         Element.B_Leg := Destination;
      end Update;
   begin
      Call_List.Update (Obj.ID, Update'Access);
   end Dial;

   ---------------
   -- End_Dial  --
   ---------------

   procedure End_Dial (Obj : in Instance) is
      procedure Update (Key     : in     Identification;
                        Element : in out Instance);

      procedure Update (Key     : in     Identification;
                        Element : in out Instance) is
         pragma Unreferenced (Key);
      begin
         Element.State := Ended;
      end Update;
   begin
      Call_List.Update (Obj.ID, Update'Access);
   end End_Dial;

   ---------------
   --  Enqueue  --
   ---------------

   procedure Enqueue (Obj : in Instance) is
   begin
      Call_List.Enqueue (Obj.ID);
   end Enqueue;

   -----------
   --  Get  --
   -----------

   function Get (Call : Identification) return Instance is
   begin
      return Call_List.Get (Call);
   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Channel : Channel_Identification) return Instance is
   begin
      return Call_List.Get (Channel);
   end Get;

   -----------
   --  Has  --
   -----------

   function Has (Channel_ID : Channel_Identification) return Boolean is
   begin
      return Call_List.Contains (Channel_ID);
   end Has;

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

   ------------
   --  Next  --
   ------------

   function Next return Identification is
   begin
      Next_Identification := Identification'Succ (Next_Identification);
      --  TODO check of already alllocated ID's

      if Next_Identification = Null_Identification then
         Next_Identification := Identification'Succ (Next_Identification);
      end if;

      return Next_Identification;
   end Next;

   --------------------
   --  Organization  --
   --------------------

   function Organization (Obj : in Instance)
                          return Model.Organization_Identifier is
   begin
      return Obj.Organization;
   end Organization;

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
      Removed_Call : Instance;
   begin
      --  Make a copy of the call to be able to return it.
      Removed_Call := Call_List.Get (ID);

      Call_List.Remove (ID);
      return Removed_Call;
   end Remove;

   --------------
   --  Remove  --
   --------------

   function Remove (Channel_ID : in Channel_Identification) return Instance is
   begin
      return Remove (Call_List.Get (Channel_ID).ID);
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
   begin
      return Ada.Strings.Fixed.Trim (Source => Item'Img,
                                     Side   => Left);
   end To_String;

   -----------------
   --  To_String  --
   -----------------

   function To_String (Channel : in Channel_Identification) return String is
   begin
      return Ada.Strings.Unbounded.To_String
        (Ada.Strings.Unbounded.Unbounded_String (Channel));
   end To_String;

   -------------
   --  Value  --
   -------------

   function Value (Item : in String) return Channel_Identification is
   begin
      return Channel_Identification
        (Ada.Strings.Unbounded.To_Unbounded_String (Item));
   exception
      when Constraint_Error =>
         raise Invalid_ID with Item;
   end Value;

   -------------
   --  Value  --
   -------------

   function Value (Item : in String) return Identification is
   begin
      return Identification (Natural'Value (Item));
   exception
      when Constraint_Error =>
         raise Invalid_ID with Item;
   end Value;
   -----------------
   --  Call_List  --
   -----------------

   protected body Call_List is

      function Contains (Channel_ID : in Channel_Identification)
                         return Boolean is
      begin
         return List.Contains (Channel_Lookup_Table.Element
                               (AMI.Channel.Channel_Key (Channel_ID)));
      end Contains;

      function Contains (ID : in Identification)
                         return Boolean is
      begin
         return List.Contains (ID);
      end Contains;

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
         Number_Queued := Number_Queued - 1;
      end Dequeue;

      function Empty return Boolean is
      begin
         return List.Is_Empty;
      end Empty;

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

      function First return Instance is
      begin
         return List.First_Element;
      end First;

      function Get (Channel : in Channel_Identification) return Instance is
      begin
         return List.Element
           (Channel_Lookup_Table.Element
              (AMI.Channel.Channel_Key (Channel)));
      exception
         when Constraint_Error =>
            raise Not_Found with " channel " & To_String (Channel);
      end Get;

      function Get (ID : in Identification) return Instance is
      begin
         return List.Element (ID);
      exception
         when Constraint_Error =>
            raise Not_Found with " call " & To_String (ID);
      end Get;

      procedure Insert (Item : Instance) is
      begin
         List.Insert (Key      => Item.ID,
                      New_Item => Item);
         Channel_Lookup_Table.Insert
           (Key      => AMI.Channel.Channel_Key (Item.Channel),
            New_Item => Item.ID);
      exception
         when Constraint_Error =>
            --  Roll back, if applicable.
            if List.Contains (Item.ID) then
               List.Delete (Item.ID);
            end if;
            raise Constraint_Error with
              " channel " & To_String (Item.Channel)
              & " ID" & To_String (Item.ID);
      end Insert;

      procedure Link (ID1 : Identification;
                      ID2 : Identification) is
         procedure Update (Key     : in     Identification;
                           Element : in out Instance);

         procedure Update (Key     : in     Identification;
                           Element : in out Instance) is
            pragma Unreferenced (Key);
         begin
            if Element.Channel /= Null_Channel_Identification then
               raise Already_Bridged with To_String (Element.Channel);
            end if;

            if Element.ID = ID1 then
               Element.B_Leg := List.Element (ID2).Channel;
            elsif Element.ID = ID2 then
               Element.B_Leg := List.Element (ID1).Channel;
            end if;
         end Update;
      begin
         --  Sanity checks.
         if ID1 = Null_Identification or ID2 = Null_Identification then
            raise Constraint_Error with "Null value detected";
         end if;

         if List.Element (ID1).B_Leg /= Null_Channel_Identification
           or List.Element (ID2).B_Leg /= Null_Channel_Identification then
            raise Already_Bridged with To_String (List.Element (ID1).Channel) &
              "<->" & To_String (List.Element (ID1).B_Leg) & "," &
              To_String (List.Element (ID2).Channel) &
              "<->" & To_String (List.Element (ID2).B_Leg);
         end if;
         Call_List.Update (ID1, Update'Access);
         Call_List.Update (ID2, Update'Access);
      end Link;

      function Queued return Natural is
      begin
         return Number_Queued;
      end Queued;

      procedure Remove (Channel_ID : in Channel_Identification) is
         Call : Identification := Null_Identification;
      begin
         Call := Channel_Lookup_Table.Element
           (AMI.Channel.Channel_Key (Channel_ID));
         List.Delete (Call);
      exception
         when Constraint_Error =>
            raise Not_Found with " channel " & To_String (Channel_ID);
      end Remove;

      procedure Remove (ID : in Identification) is
      begin
         Channel_Lookup_Table.Delete
           (AMI.Channel.Channel_Key (List.Element (ID).Channel));
         List.Delete (ID);
      exception
         when Constraint_Error =>
            raise Not_Found with " call " & To_String (ID);
      end Remove;

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