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
private with Ada.Strings.Unbounded;
with GNATCOLL.JSON;

with ESL.UUID;
with Common;

package Model.Call is
   use Common;
   use ESL.UUID;

   Package_Name : constant String := "Model.Call";

   Not_Found       : exception;
   Null_Channel    : exception;
   Null_ID         : exception;
   Not_Available   : exception;
   Invalid_ID      : exception;
   --  Appropriately named exceptions.

   subtype Identification is ESL.UUID.Instance;
   --  Call identification.

   function "=" (Left, Right : in Identification) return Boolean;

   function To_String (Item : in Identification) return String;
   function Image (Item : in Identification) return String renames To_String;
   --  Image function.

   function Value (Item : String) return Identification;
   --  Conversion.

   type States is
     (Unknown,
      Created,
      Ringing,
      Queued,
      Left_Queue,
      Hungup,
      Transferring,
      Transferred,
      Speaking,
      Parked,
      Unparked);
   --  Valid states for the call.

   type Priority_Level is (Invalid, Low, Normal, High);
   --  Priority level inherited from the base organization priority.

   type Instance is tagged private;
   --  Call instance.

   function ID (Obj : in Instance) return Model.Call.Identification;
   function State (Obj : in Instance) return States;
   function Inbound (Obj : in Instance) return Boolean;
   function Extension (Obj : in Instance) return String;
   function From_Extension (Obj : in Instance) return String;
   function B_Leg (Obj : in Instance) return Identification;
   function Arrival_Time (Obj : in Instance) return Common.Time;
   function Assigned_To (Obj : in Instance) return Natural;
   function Reception_ID (Obj : in Instance) return Reception_Identifier;
   function Greeting_Played (Obj : in Instance) return Boolean;
   --  Accessor methods

   procedure Set_Reception_ID (Obj  : in Instance;
                               R_ID : in Reception_Identifier);

   procedure Set_Outbound_Parameters
        (Item : in Instance;
         R_ID : in Reception_Identifier;
         C_ID : in Contact_Identifier;
         U_ID : in Model.User_Identifier);

   procedure Change_State (Obj  : in Instance;
                           New_State : in States);

   procedure Mark_As_Call (Obj : in Instance);
   --  Mutator methods.

   procedure Link (ID_1, ID_2 : in Identification);
   procedure Unlink (ID : in Identification);

   procedure Lock (Obj : in Instance);
   procedure Unlock (Obj : in Instance);

   function List_Empty return Boolean;
   function List return GNATCOLL.JSON.JSON_Value;
   function Queued_Calls return GNATCOLL.JSON.JSON_Value;
   procedure For_Each (Process : access procedure (Item : Instance)) is null;
   function Queue_Count return Natural;

   function Get (Call : Identification) return Instance;

   function Has (ID : Identification) return Boolean;

   function Queue_Empty return Boolean;
   --  Reveals if there are currently calls available for pickup.

   --  ^Collection operations.

   function Null_Instance return Instance;

   function Null_Identification return Identification;
   --  ^Explicit null values.

   procedure Assign_Call
     (To   : in     Model.User_Identifier;
      Call :    out Model.Call.Instance;
      ID   : in     Model.Call.Identification :=
        Model.Call.Null_Identification);

   procedure Release (Call : in Model.Call.Instance);

   procedure Create_And_Insert
     (Inbound         : in Boolean;
      ID              : in Identification;
      Reception_ID    : in Reception_Identifier;
      Extension       : in String := "";
      From_Extension  : in String := "");

   --  ^Constructors

   function To_JSON (Obj : in Instance) return GNATCOLL.JSON.JSON_Value;
   --  TODO: Move this to the view package.

private
   use Ada.Strings.Unbounded;

   type Instance is tagged
      record
         ID              : Identification;
         State           : States;
         Is_Call         : Boolean := False;
         Greeting_Played : Boolean := False;
         Locked          : Boolean := False;
         Inbound         : Boolean;
         Extension       : Unbounded_String;
         Reception_ID    : Reception_Identifier := Null_Reception_Identifier;
         Assigned_To     : Model.User_Identifier;
         From_Extension  : Unbounded_String;
         B_Leg           : Identification;
         Arrived         : Time := Current_Time;
      end record;

   package Call_Storage is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Identification,
        Element_Type => Instance);

   protected Call_List is
      procedure Assign_Call
        (To   : in     Model.User_Identifier;
         Call :    out Model.Call.Instance;
         ID   : in     Model.Call.Identification
                          := Model.Call.Null_Identification);

      procedure Insert (Item : in Instance);

      procedure Set_Outbound_Parameters
        (Item : in Instance;
         R_ID : in Reception_Identifier;
         C_ID : in Contact_Identifier;
         U_ID : in Model.User_Identifier);

      function Empty return Boolean;
      procedure Change_State (ID        : in Identification;
                              New_State : in States);
      function Contains (ID : in Identification) return Boolean;

      function Get (ID : in Identification) return Instance;
      procedure Link (ID_1 : in Identification;
                      ID_2 : in Identification);
      procedure Release (Call : in Model.Call.Instance);

      procedure Set_Call (ID      : in Identification;
                          Is_Call : in Boolean);
      procedure Set_Locked (ID     : in Identification;
                            Locked : in Boolean);
      procedure Set_Reception (ID   : in Identification;
                               R_ID : in Reception_Identifier);
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

end Model.Call;
