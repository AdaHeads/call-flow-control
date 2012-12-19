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

with Model.Call_ID;
with AMI.Channel_ID;
with AMI.Parser;
with Common;

package AMI.Channel is
   use Model.Call_ID;
   use Model;

   type Valid_State is (Unknown, Down, Reserved, Off_Hook,
                        Dialed, Ringing, Receiver_Ringing, Up, Busy);

   package US renames Ada.Strings.Unbounded;

   type Instance (Is_Null : Boolean) is tagged record
      case Is_Null is
         when False =>
            ID                    : Channel_ID.Instance (Temporary => False);
            Bridged_With          : Channel_ID.Instance (Temporary => False);
            State                 : Valid_State;
            Priority              : Natural;
            Unique_ID             : Call_ID_Type;
            Caller_ID_Number      : US.Unbounded_String;
            Caller_ID_Name        : US.Unbounded_String;
            Connected_Line_Number : US.Unbounded_String;
            Connected_Line_Name   : US.Unbounded_String;
            Account_Code          : US.Unbounded_String;
            Extension             : US.Unbounded_String;
            Application           : US.Unbounded_String;
            Application_Data      : US.Unbounded_String;
            Context               : US.Unbounded_String;
            Bridged_Unique_ID     : Call_ID.Call_ID_Type;
            Created_At            : Common.Time;
         when True =>
            null;
      end case;
   end record;
   --  Representation of a channel.

   function Create (Packet : in AMI.Parser.Packet_Type) return Instance;
   --  Robust constructor that ignores, non-important missing fields.

   procedure Change_State (Channel :    out Instance;
                           Packet  : in     AMI.Parser.Packet_Type);
   --  Updates the state of a channel with the values from the given
   --  Packet.

   function To_JSON (Channel : in Instance)
                     return GNATCOLL.JSON.JSON_Value;

   function To_String (Channel : in Instance) return String;

   Null_Object  : constant Instance;
   Empty_Object : constant Instance;

   function To_Channel_State (Item : in String) return Valid_State;
   function To_Channel_State (Item : in Integer) return Valid_State;
   --  Conversion functions.

      Not_Found     : exception;
   Duplicate_Key : exception;

   type Channel_Process_Type is not null access
     procedure (C : in AMI.Channel.Instance);

   package Channel_List_Type is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => Channel_ID.Instance,
        Element_Type => AMI.Channel.Instance,
        "<"          => Channel_ID."<",
        "="          => AMI.Channel."=");

   protected type Protected_Channel_List_Type is
      function Contains (Key : in Channel_ID.Instance) return Boolean;
      procedure Insert (Item : in AMI.Channel.Instance);
      procedure Remove (Key : in Channel_ID.Instance);
      function Get (Key : in Channel_ID.Instance) return AMI.Channel.Instance;
      function Length return Natural;
      function To_JSON return GNATCOLL.JSON.JSON_Value;
      function To_String return String;
      procedure Update (Item : in AMI.Channel.Instance);
   private
      Protected_List : Channel_List_Type.Map;
   end Protected_Channel_List_Type;

   List : Protected_Channel_List_Type;
   --  Package-visible singleton.

private
   Null_Object : constant Instance := (Is_Null => True);
   Empty_Object : constant Instance :=
                    (Is_Null               => False,
                     ID                    => Channel_ID.Null_Channel_ID,
                     Bridged_With          => Channel_ID.Null_Channel_ID,
                     State                 => Unknown,
                     Priority              => 0,
                     Caller_ID_Number      => US.Null_Unbounded_String,
                     Caller_ID_Name        => US.Null_Unbounded_String,
                     Connected_Line_Number => US.Null_Unbounded_String,
                     Connected_Line_Name   => US.Null_Unbounded_String,
                     Account_Code          => US.Null_Unbounded_String,
                     Application           => US.Null_Unbounded_String,
                     Application_Data      => US.Null_Unbounded_String,
                     Unique_ID             => Call_ID.Null_Call_ID,
                     Bridged_Unique_ID     => Call_ID.Null_Call_ID,
                     Extension             => US.Null_Unbounded_String,
                     Context               => US.Null_Unbounded_String,
                     Created_At            => Common.Current_Time);

   --  TODO: Implement these
   --     package Channel_Storage is new
   --       Ada.Containers.Indefinite_Ordered_Maps
   --         (Key_Type     => Channel_ID.Instance,
   --          Element_Type => Channel.Instance,
   --          "<"          => Channel_ID."<",
   --          "="          => Channel."=");
   --
   --     Channel_List : Channel_Storage.Map := Channel_Storage.Empty_Map;

end AMI.Channel;
