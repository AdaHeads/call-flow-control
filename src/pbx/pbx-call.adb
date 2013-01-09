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

   --------------
   --  Assign  --
   --------------

   procedure Assign (Obj : in Instance; To : in Agent_ID_Type) is
   begin
      null;
   end Assign;

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
      Calls.Insert (Item => Call);
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
      Calls.Insert (Item => Call);
   end Create_And_Insert;

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
      Calls.Update (Obj.ID, Update'Access);
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
      Calls.Update (Obj.ID, Update'Access);
   end End_Dial;

   -----------
   --  Get  --
   -----------

   function Get (Call : Identification) return Instance is
   begin
      return Calls.Get (Call);
   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Channel : Channel_Identification) return Instance is
   begin
      return Calls.Get (Channel);
   end Get;

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

   function To_String (Channel : in Channel_Identification) return String is
   begin
      return Ada.Strings.Unbounded.To_String
        (Ada.Strings.Unbounded.Unbounded_String (Channel));
   end To_String;

   function Value (Item : String) return Channel_Identification is
   begin
      return Channel_Identification
        (Ada.Strings.Unbounded.To_Unbounded_String (Item));
   end Value;

   function ID (Obj : in Instance) return Identification is
   begin
      return Obj.ID;
   end ID;

   function State (Obj : in Instance) return States is
   begin
      return Obj.State;
   end State;

   function Inbound (Obj : in Instance) return Boolean is
   begin
      return Obj.Inbound;
   end Inbound;

   function Channel (Obj : in Instance) return Channel_Identification is
   begin
      return Obj.Channel;
   end Channel;

   function B_Leg (Obj : in Instance) return Channel_Identification is
   begin
      return Obj.B_Leg;
   end B_Leg;

   function Organization (Obj : in Instance)
                          return Model.Organization_Identifier is
   begin
      return Obj.Organization;
   end Organization;

   function Assigned_To (Obj : in Instance)
                         return Model.Agent_ID.Agent_ID_Type is
   begin
      return Obj.Assigned_To;
   end Assigned_To;

   function Arrival_Time (Obj : in Instance) return Common.Time is
   begin
      return Obj.Arrived;
   end Arrival_Time;

   protected body Calls is
      function Get (Channel : in Channel_Identification) return Instance is
      begin
         return List.Element
           (Channel_Lookup_Table.Element
              (AMI.Channel.Channel_Key (Channel)));
      exception
         when Constraint_Error =>
            raise Not_Found with " channel " & To_String (Channel);
      end Get;

      function Get (Call : Identification) return Instance is
      begin
         return List.Element (Call);
      exception
         when Constraint_Error =>
            raise Not_Found with " call " & To_String (Call);
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
            raise;
      end Insert;

      procedure Remove (Channel : in Channel_Identification) is
         Call : Identification := Null_Identification;
      begin
         Call := Channel_Lookup_Table.Element
           (AMI.Channel.Channel_Key (Channel));
         List.Delete (Call);
      exception
         when Constraint_Error =>
            raise Not_Found with " channel " & To_String (Channel);
      end Remove;

      procedure Remove (Call : Identification) is
      begin
         Channel_Lookup_Table.Delete
           (AMI.Channel.Channel_Key (List.Element (Call).Channel));
         List.Delete (Call);
      exception
         when Constraint_Error =>
            raise Not_Found with " call " & To_String (Call);
      end Remove;

      procedure Update (Call    : in Identification;
                        Process : not null access procedure
                          (Key     : in     Identification;
                           Element : in out Instance)) is
      begin
         List.Update_Element (Position => List.Find (Call),
                              Process  => Process);
      end Update;

   end Calls;

end PBX.Call;
