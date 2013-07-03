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
with Ada.Strings.Unbounded;

with GNATCOLL.JSON;

with ESL.Channel;
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

   type Identification is new ESL.Channel.Channel_Key;
   --  Call identification.

   function To_String (Item : in Identification) return String;
   function Image (Item : in Identification) return String renames To_String;
   --  Image function.

   function Value (Item : String) return Identification;
   --  Conversion.

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
   function B_Leg (Obj : in Instance) return Identification;
   function Organization (Obj : in Instance)
                          return Model.Organization_Identifier;
   function Assigned_To (Obj : in Instance)
                         return Model.Agent_ID.Agent_ID_Type;
   function Arrival_Time (Obj : in Instance) return Common.Time;
   --  Accessor methods

   procedure Assign (Obj : in Instance; To : in Agent_ID_Type);

   procedure Change_State (Obj : in Instance; New_State : in States);

   --  Mutator methods.

   procedure Link (ID_1, ID_2 : in Identification);

   procedure Unlink (ID : in Identification);

   function List_Empty return Boolean;
   function List return GNATCOLL.JSON.JSON_Value;
   function Queued_Calls return GNATCOLL.JSON.JSON_Value;
   procedure For_Each (Process : access procedure (Item : Instance)) is null;
   function Queue_Count return Natural;

   function Get (Call : Identification) return Instance;

   function Has (ID : Identification) return Boolean;

   function Highest_Prioirity return Instance;

   function Remove (ID : in Identification) return Instance;

   function Queue_Empty return Boolean;
   --  Reveals if there are currently calls available for pickup.

   --  ^Collection operations.

   Null_Instance               : constant Instance;
   Null_Identification         : constant Identification;
   --  ^Explicit null values.

   procedure Create_And_Insert
     (Inbound         : in Boolean;
      ID              : in Identification;
      State           : in States := Unknown;
      Organization_ID : in Natural := 0;
      Assigned_To     : in Agent_ID_Type := Null_Agent_ID);

   function Allocate
     (Assigned_To : in Agent_ID_Type) return Identification;
   --  Allocates a call without a channel but assigning it to an agent, and
   --  giving it a call ID.

   --  ^Constructors

   function To_JSON (Obj : in Instance) return GNATCOLL.JSON.JSON_Value;
   --  TODO: Move this to the view package.

private

   Null_Identification : constant Identification
     := Identification (Ada.Strings.Unbounded.Null_Unbounded_String);
   Next_Identification : Identification := Null_Identification;

   type Instance is tagged
      record
         ID           : Identification;
         State        : States;
         Inbound      : Boolean;
         Organization : Model.Organization_Identifier;
         B_Leg        : Identification;
         Arrived      : Time := Current_Time;
         Assigned_To  : Agent_ID_Type := Null_Agent_ID;
      end record;

   Null_Instance : constant Instance :=
                     (ID           => Null_Identification,
                      State        => States'First,
                      Inbound      => False,
                      Organization => Model.Organization_Identifier (0),
                      B_Leg        => Null_Identification,
                      Arrived      => Common.Null_Time,
                      Assigned_To  => Null_Agent_ID);

   package Call_Storage is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Identification,
        Element_Type => Instance);

   protected Call_List is
      procedure Assign_To (ID       : in Identification;
                           Agent_ID : in Agent_ID_Type);
      procedure Insert (Item : in Instance);
      function Empty return Boolean;
      procedure Change_State (ID        : in Identification;
                              New_State : in States);
      function Contains (ID : in Identification) return Boolean;
      procedure Enqueue (ID : in Identification);
      pragma Obsolescent (Enqueue);
      procedure Dequeue (ID : in Identification);
      pragma Obsolescent (Dequeue);
      function First return Instance;

      function Get (ID : in Identification) return Instance;
      procedure Link (ID_1 : in Identification;
                      ID_2 : in Identification);
      procedure Unlink (ID : in Identification);
      function Queued return Natural;
      procedure Remove (ID : in Identification);

      function To_JSON (Only_Queued : Boolean := False)
                        return GNATCOLL.JSON.JSON_Value;
      procedure Update (ID : in Identification;
                        Process : not null access procedure
                          (Key     : in     Identification;
                           Element : in out Instance));
      pragma Obsolescent (Update);
   private
      List                 : Call_Storage.Map;
      Number_Queued        : Natural := 0;
   end Call_List;

end PBX.Call;
