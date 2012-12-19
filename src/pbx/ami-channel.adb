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

with Ada.Characters.Handling;
with Ada.Calendar.Formatting;
with Ada.Calendar;

package body AMI.Channel is
   use type Channel_ID.Instance;

   protected body Protected_Channel_List_Type is
      function Contains (Key : in Channel_ID.Instance) return Boolean is
      begin
         return Protected_List.Contains (Key);
      end Contains;

      function Get (Key : in Channel_ID.Instance)
                    return AMI.Channel.Instance is
         Item : constant AMI.Channel.Instance :=
           Protected_List.Element (Key => Key);
      begin
         if Item = AMI.Channel.Null_Object then
            raise Not_Found;
         end if;

         return Item;
      end Get;

      --  Places the Channel, in the queue.
      procedure Insert (Item : in AMI.Channel.Instance) is
      begin
         Protected_List.Insert
           (Key       => Item.ID,
            New_Item  => Item);
      exception
         when Constraint_Error =>
            if Protected_List.Element (Key => Item.ID).ID = Item.ID then
               raise Duplicate_Key with "key " & Item.ID.Image &
                 " already in map.";
            end if;
            raise;
      end Insert;

      --  Returns the total number of Channels.
      function Length return Natural is
         use Ada.Containers;
      begin
         return Natural (Protected_List.Length);
      end Length;

      --  Removes the Channel with the specified UniqueID
      procedure Remove (Key : Channel_ID.Instance) is
      begin
         Protected_List.Delete (Key);
      end Remove;

      function To_JSON return GNATCOLL.JSON.JSON_Value is
         use GNATCOLL.JSON;
         Value     : JSON_Value := Create_Object;
         JSON_List : JSON_Array;
         Root      : constant JSON_Value := Create_Object;
      begin
         for Item of Protected_List loop
            if Item /= AMI.Channel.Null_Object then
               Value := Item.To_JSON;
               Append (JSON_List, Value);
            end if;
         end loop;
         Root.Set_Field ("channels", JSON_List);
         return Root;
      end To_JSON;

      function To_String return String is
         use Ada.Strings.Unbounded;
         Item : Unbounded_String;
      begin
         for Channel of Protected_List loop
            Append (Item, Channel.To_String);
         end loop;
         return To_String (Item);
      end To_String;

      procedure Update (Item : in AMI.Channel.Instance) is
      begin
         if Protected_List.Contains (Item.ID) then
            Protected_List.Replace (Key      => Item.ID,
                                    New_Item => Item);
         else
            raise Not_Found;
         end if;
      end Update;
   end Protected_Channel_List_Type;

   --------------------
   --  Change_State  --
   --------------------

   procedure Change_State (Channel :    out Instance;
                           Packet  : in     AMI.Parser.Packet_Type) is
   begin
      Channel.State :=
        To_Channel_State (Packet.Get_Value (AMI.Parser.ChannelState));
      Channel.Caller_ID_Number :=
        Packet.Get_Value (AMI.Parser.CallerIDNum);
      Channel.Caller_ID_Name :=
        Packet.Get_Value (AMI.Parser.CallerIDName);
      Channel.Connected_Line_Number :=
        Packet.Get_Value (AMI.Parser.ConnectedLineNum);
      Channel.Connected_Line_Name :=
        Packet.Get_Value (AMI.Parser.ConnectedLineName);
   end Change_State;

   --------------
   --  Create  --
   --------------

   function Create (Packet : in AMI.Parser.Packet_Type) return Instance is
      use Ada.Calendar;

      Bridged_Channel   : Channel_ID.Instance  := Channel_ID.Null_Channel_ID;
      Bridged_Unique_ID : Call_ID.Call_ID_Type := Call_ID.Null_Call_ID;
      Created_At        : Common.Time          := Common.Current_Time;
      Time_Active       : Duration             := 0.0;
      Priority          : Natural              := 0;
      Extension         : US.Unbounded_String  := US.Null_Unbounded_String;
   begin
      if not
        Channel_ID.Validate (Packet.Get_Value (AMI.Parser.Channel))
      then
         return Null_Object;
      end if;

      if Packet.Get_Value (Key      => AMI.Parser.BridgedChannel,
                          Required => False) /= ""
      then
         Bridged_Channel := Channel_ID.Value
           (Packet.Get_Value (AMI.Parser.BridgedChannel));
      end if;

      if Packet.Get_Value (Key      => AMI.Parser.BridgedUniqueID,
                           Required => False) /= ""
      then
         Bridged_Unique_ID := Call_ID.Create
           (Packet.Get_Value (AMI.Parser.BridgedUniqueID));
      end if;

      --  Horribly inconsistant naming conflict fix.
      if Packet.Has_Value (AMI.Parser.Exten) then
         Extension := Packet.Get_Value (AMI.Parser.Exten);
      elsif Packet.Has_Value (AMI.Parser.Extension) then
         Extension := Packet.Get_Value (AMI.Parser.Extension);
      end if;

      if Packet.Has_Value (AMI.Parser.Duration) then
         Time_Active := Ada.Calendar.Formatting.Value
           (Elapsed_Time => Packet.Get_Value (AMI.Parser.Duration));

         Created_At := Created_At - Time_Active;
      end if;

      if Packet.Get_Value (AMI.Parser.ChannelState, False) /= "" then
         Priority :=
           Natural'Value (Packet.Get_Value (AMI.Parser.ChannelState, False));
      end if;

      return
        (Is_Null               => False,
         ID                    =>
           Channel_ID.Value (Packet.Get_Value (AMI.Parser.Channel)),
         Bridged_With          =>
           Bridged_Channel,
         State                 =>
           To_Channel_State (Packet.Get_Value (AMI.Parser.ChannelState)),
         Priority              =>
           Priority,
         Caller_ID_Number      =>
           Packet.Get_Value (AMI.Parser.CallerIDNum),
         Caller_ID_Name        =>
           Packet.Get_Value (AMI.Parser.CallerIDName),
         Connected_Line_Number =>
           Packet.Get_Value (AMI.Parser.ConnectedLineNum, False),
         Connected_Line_Name   =>
           Packet.Get_Value (AMI.Parser.ConnectedLineName, False),
         Account_Code          =>
           Packet.Get_Value (AMI.Parser.AccountCode),
         Application           =>
           Packet.Get_Value (AMI.Parser.Application, False),
         Application_Data      =>
           Packet.Get_Value (AMI.Parser.ApplicationData, False),
         Unique_ID             =>
           Call_ID.Create (Packet.Get_Value (AMI.Parser.UniqueID)),
         Bridged_Unique_ID     =>
           Bridged_Unique_ID,
         Extension             =>
           Extension,
         Context               =>
           Packet.Get_Value (AMI.Parser.Context),
         Created_At            => Created_At);
   end Create;

   function To_Channel_State (Item : in String) return Valid_State is
   begin
      return Valid_State'Val (Integer'Value (Item));
   end To_Channel_State;

   function To_Channel_State (Item : in Integer) return Valid_State is
   begin
      return Valid_State'Val (Item);
   end To_Channel_State;

   function To_JSON (Channel : in Instance)
                     return GNATCOLL.JSON.JSON_Value is
      use Common;
      use GNATCOLL.JSON;
      use Ada.Characters.Handling;

      JSON         : constant JSON_Value := Create_Object;
      Channel_JSON : constant JSON_Value := Create_Object;
   begin
      Channel_JSON.Set_Field
        ("ID", Channel.ID.Image);
      Channel_JSON.Set_Field
        ("Unique_ID", Channel.Unique_ID.To_String);
      Channel_JSON.Set_Field
        ("State", To_Lower (Channel.State'Img));
      Channel_JSON.Set_Field
        ("Caller_ID_Name", US.To_String (Channel.Caller_ID_Name));
      Channel_JSON.Set_Field
        ("Caller_ID_Number", US.To_String (Channel.Caller_ID_Number));
      Channel_JSON.Set_Field
        ("Account_Code", US.To_String (Channel.Account_Code));
      Channel_JSON.Set_Field ("Extension", US.To_String (Channel.Extension));
      Channel_JSON.Set_Field ("Context", US.To_String (Channel.Context));
      JSON.Set_Field ("channel", Channel_JSON);
      JSON.Set_Field ("timestamp", Unix_Timestamp (Current_Time));

      return JSON;
   end To_JSON;

   function To_String (Channel : in Instance) return String is
   begin
      return
        "ID => "                & Channel.ID.Image & ", " &
        "Unique_ID => "         & Channel.Unique_ID.To_String & ", " &
        "State => "             & Channel.State'Img & ", " &
        "Caller_ID_Number => "
        & US.To_String (Channel.Caller_ID_Number) & ", " &
        "Caller_ID_Name => "
        & US.To_String (Channel.Caller_ID_Name) & ", " &
        "Account_Code => "      & US.To_String (Channel.Account_Code) & ", " &
        "Extension => "         & US.To_String (Channel.Extension) & ", " &
        "Context => "           & US.To_String (Channel.Context);
   end To_String;
end AMI.Channel;
