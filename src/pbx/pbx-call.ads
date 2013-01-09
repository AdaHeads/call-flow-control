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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;

with GNATCOLL.JSON;

with AMI.Channel;
with Model.Agent_ID;
with Model;
with Common;

package PBX.Call is
   use Common;
   use Model.Agent_ID;

   Not_Found : exception;

   type Identification is private;

   function To_String (Item : in Identification) return String;

   type Channel_Identification is new AMI.Channel.Channel_Key;

   function Value (Item : String) return Channel_Identification;

   Null_Channel_Identification : constant Channel_Identification
     := Channel_Identification (Ada.Strings.Unbounded.Null_Unbounded_String);

   function To_String (Channel : in Channel_Identification) return String;

   type States is
     (Unknown, Queued, Requeued,
      Speaking, Dialing, OnHold, Delegated, Ended,
      Parked, Bridged);
   type Priority_Level is (Invalid, Low, Normal, High);

   type Instance is tagged private;

   --  Accessor methods

   function ID (Obj : in Instance) return Identification;
   function State (Obj : in Instance) return States;
   function Inbound (Obj : in Instance) return Boolean;
   function Channel (Obj : in Instance) return Channel_Identification;
   function B_Leg (Obj : in Instance) return Channel_Identification;
   function Organization (Obj : in Instance)
                          return Model.Organization_Identifier;
   function Assigned_To (Obj : in Instance)
                         return Model.Agent_ID.Agent_ID_Type;
   function Arrival_Time (Obj : in Instance) return Common.Time;

   --  Mutator methods

   procedure Assign (Obj : in Instance; To : in Agent_ID_Type);

   procedure Dial (Obj : in Instance; Destination : in Channel_Identification);

   procedure End_Dial (Obj : in Instance);

   procedure For_Each (Process : access procedure (Item : Instance)) is null;

   Null_Identification : constant Identification;

   function Get (Channel : Channel_Identification) return Instance;

   function Get (Call : Identification) return Instance;

   --  Constructors

   function Create_And_Insert
     (Inbound         : in Boolean;
      Channel         : in String;
      State           : in States := Unknown;
      Organization_ID : in Natural := 0;
      B_Leg           : in String := "")
      return Instance;

   procedure Create_And_Insert
     (Inbound         : in Boolean;
      Channel         : in String;
      State           : in States := Unknown;
      Organization_ID : in Natural := 0;
      B_Leg           : in String := "");

   function To_JSON (Obj : in Instance) return GNATCOLL.JSON.JSON_Value;

   function Next return Identification;

   --    type Subscribeable_Event is (Join_Queue, Leave_Queue);

   --     type Event (Kind : Subscribeable_Event) is record
   --        case Kind is
   --           when Join_Queue =>
   --              New_Call : Model.Call.Call_Type;
   --           when others =>
   --              null;
   --        end case;
   --     end record;

private
   type Identification is mod 2 ** 32;

   Null_Identification : constant Identification := Identification'First;

   Next_Identification : Identification := Null_Identification;

   type Instance is tagged
      record
         ID           : Identification;
         State        : States;
         Inbound      : Boolean;
         Channel      : Channel_Identification;
         Organization : Model.Organization_Identifier;
         B_Leg        : Channel_Identification;
         Arrived      : Time := Current_Time;
         Assigned_To  : Agent_ID_Type := Create ("");
      end record;

   package Call_Storage is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Identification,
        Element_Type => Instance);

   package Channel_Lookup_Storage is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type     => AMI.Channel.Channel_Key,
        Element_Type => Identification,
        Hash           => Ada.Strings.Unbounded.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Unbounded."=");

   protected Calls is
      procedure Insert (Item : Instance);
      function Get (Channel : in Channel_Identification) return Instance;
      function Get (Call : Identification) return Instance;
      procedure Remove (Channel : in Channel_Identification);
      procedure Remove (Call : Identification);
      procedure Update (Call    : in Identification;
                        Process : not null access procedure
                          (Key     : in     Identification;
                           Element : in out Instance));
   private
      Channel_Lookup_Table : Channel_Lookup_Storage.Map;
      List                 : Call_Storage.Map;
   end Calls;

end PBX.Call;
