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

   Package_Name : constant String := "PBX.Call";

   Not_Found       : exception;
   Null_Channel    : exception;
   Null_ID         : exception;
   Already_Bridged : exception;
   Invalid_ID      : exception;
   --  Appropriately named exceptions.

   type Identification is private;
   --  Call identification.

   function To_String (Item : in Identification) return String;
   function Image (Item : in Identification) return String renames To_String;
   --  Image function.

   function Value (Item : String) return Identification;
   --  Conversion.

   type Channel_Identification is new AMI.Channel.Channel_Key;
   --  PBX channel identification, or call leg.

   function Value (Item : String) return Channel_Identification;
   --  Conversion.

   function To_String (Channel : in Channel_Identification) return String;
   function Image (Channel : in Channel_Identification) return String
                   renames To_String;
   --  Image function.

   type States is
     (Unknown, Pending,
      Created,
      Queued,
      IVR,
      Transferring,
      Speaking, Dialing, Delegated, Ended,
      Parked, Transferred);
   --  Valid states for the call.

   type Priority_Level is (Invalid, Low, Normal, High);
   --  Priority level inherited from the base organization priority.

   type Instance is tagged private;
   --  Call instance.

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
   --  Accessor methods

   procedure Assign (Obj : in Instance; To : in Agent_ID_Type);

   procedure Channel (Obj        : in Instance;
                      Channel_ID : in Channel_Identification);

   procedure Dial (Obj : in Instance; Destination : in Channel_Identification);

   procedure Dequeue (Obj : in Instance);
   pragma Obsolescent (Dequeue);
   procedure Enqueue (Obj : in Instance);
   pragma Obsolescent (Enqueue);

   procedure End_Dial (Obj : in Instance);
   pragma Obsolescent (End_Dial);

   procedure Change_State (Obj : in Instance; New_State : in States);

   --  Mutator methods.

   function List_Empty return Boolean;
   function List return GNATCOLL.JSON.JSON_Value;
   function Queued_Calls return GNATCOLL.JSON.JSON_Value;
   procedure For_Each (Process : access procedure (Item : Instance)) is null;
   function Queue_Count return Natural;

   function Get (Channel : Channel_Identification) return Instance;
   function Get (Call : Identification) return Instance;

   function Has (Channel_ID : Channel_Identification) return Boolean;
   function Has (ID : Identification) return Boolean;

   function Highest_Prioirity return Instance;

   function Remove (ID : in Identification) return Instance;
   function Remove (Channel_ID : in Channel_Identification) return Instance;

   function Queue_Empty return Boolean;
   --  Reveals if there are currently calls available for pickup.

   --  ^Collection operations.

   Null_Instance               : constant Instance;
   Null_Identification         : constant Identification;
   Null_Channel_Identification : constant Channel_Identification;
   --  ^Explicit null values.

   function Create_And_Insert
     (Inbound         : in Boolean;
      Channel         : in Channel_Identification :=
        Null_Channel_Identification;
      State           : in States := Unknown;
      Organization_ID : in Natural := 0;
      Assigned_To     : in Agent_ID_Type := Null_Agent_ID;
      B_Leg           : in Channel_Identification :=
        Null_Channel_Identification)
      return Instance;

   procedure Create_And_Insert
     (Inbound         : in Boolean;
      Channel         : in Channel_Identification :=
        Null_Channel_Identification;
      State           : in States := Unknown;
      Organization_ID : in Natural := 0;
      Assigned_To     : in Agent_ID_Type := Null_Agent_ID;
      B_Leg           : in Channel_Identification :=
        Null_Channel_Identification);

   function Allocate
     (Assigned_To : in Agent_ID_Type) return Identification;
   --  Allocates a call without a channel but assigning it to an agent, and
   --  giving it a call ID.

   --  ^Constructors

   procedure Confirm
     (ID              : in Identification;
      Channel         : in Channel_Identification);
   --  Confirms a previously allocated call by giving it a channel.

   function To_JSON (Obj : in Instance) return GNATCOLL.JSON.JSON_Value;
   --  TODO: Move this to the view package.

private
   type Identification is mod 2 ** 32;

   Null_Identification : constant Identification := Identification'First;
   Next_Identification : Identification := Null_Identification;
   pragma Atomic (Next_Identification);

   Null_Channel_Identification : constant Channel_Identification
     := Channel_Identification (Ada.Strings.Unbounded.Null_Unbounded_String);

   type Instance is tagged
      record
         ID           : Identification;
         State        : States;
         Inbound      : Boolean;
         Channel      : Channel_Identification;
         Organization : Model.Organization_Identifier;
         B_Leg        : Channel_Identification;
         Arrived      : Time := Current_Time;
         Assigned_To  : Agent_ID_Type := Null_Agent_ID;
      end record;

   Null_Instance : constant Instance :=
                     (ID           => Null_Identification,
                      State        => States'First,
                      Inbound      => False,
                      Channel      => Null_Channel_Identification,
                      Organization => Model.Organization_Identifier (0),
                      B_Leg        => Null_Channel_Identification,
                      Arrived      => Common.Null_Time,
                      Assigned_To  => Null_Agent_ID);

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

   protected Call_List is
      procedure Assign_To (ID       : in Identification;
                           Agent_ID : in Agent_ID_Type);
      procedure Insert (Item : in Instance);
      function Empty return Boolean;
      procedure Change_State (ID        : in Identification;
                              New_State : in States);
      function Contains
        (Channel_ID : in Channel_Identification) return Boolean;
      function Contains (ID : in Identification) return Boolean;
      procedure Enqueue (ID : in Identification);
      pragma Obsolescent (Enqueue);
      procedure Dequeue (ID : in Identification);
      pragma Obsolescent (Dequeue);
      function Get (Channel : in Channel_Identification) return Instance;
      function First return Instance;

      function Get (ID : in Identification) return Instance;
      procedure Link (ID1 : in Identification;
                      ID2 : in Identification);
      function Queued return Natural;
      procedure Remove (Channel_ID : in Channel_Identification);
      procedure Remove (ID : in Identification);

      procedure Set_Channel (ID         : in Identification;
                             Channel_ID : in Channel_Identification);
      function To_JSON (Only_Queued : Boolean := False)
                        return GNATCOLL.JSON.JSON_Value;
      procedure Update (ID : in Identification;
                        Process : not null access procedure
                          (Key     : in     Identification;
                           Element : in out Instance));
   private
      Channel_Lookup_Table : Channel_Lookup_Storage.Map;
      List                 : Call_Storage.Map;
      Number_Queued        : Natural := 0;
   end Call_List;

end PBX.Call;
