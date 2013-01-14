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
      function Contains (Key : in Channel_Key) return Boolean is
      begin
         return Protected_List.Contains (Key);
      end Contains;

      function Get (Key : in Channel_Key)
                    return AMI.Channel.Instance is
      begin
         return Protected_List.Element (Key => Key);

      exception
         when Constraint_Error =>
            raise Not_Found with US.To_String (Key);
      end Get;

      --  Places the Channel in the map.
      procedure Insert (Key  : in Channel_Key;
                        Item : in AMI.Channel.Instance) is
      begin
         Protected_List.Insert
           (Key       => Key,
            New_Item  => Item);
      exception
         when Constraint_Error =>
            raise Duplicate_Key with "key " & Item.ID.Image &
              " already in map.";
      end Insert;

      --  Returns the total number of Channels.
      function Length return Natural is
         use Ada.Containers;
      begin
         return Natural (Protected_List.Length);
      end Length;

      procedure Put (Key  : in Channel_Key;
                        Item : in AMI.Channel.Instance) is
      begin
         if not Protected_List.Contains (Key => Key) then
            Protected_List.Insert
              (Key       => Key,
               New_Item  => Item);
         else
            Protected_List.Replace
              (Key       => Key,
               New_Item  => Item);

         end if;
      exception
         when Constraint_Error =>
            raise Duplicate_Key with "key " & Item.ID.Image &
              " already in map.";
      end Put;

      --  Removes the Channel with the specified UniqueID
      procedure Remove (Key : Channel_Key) is
      begin
         Protected_List.Delete (Key);
      end Remove;

      procedure Rename (Old_Name : in Channel_Key;
                        New_Name : in Channel_Key) is
         Tmp_Channel : Instance := Protected_List.Element (Old_Name);
      begin
         Tmp_Channel.ID := Channel_ID.Value (US.To_String (New_Name));
         Protected_List.Insert (Key      => New_Name,
                                New_Item => Tmp_Channel);
         Protected_List.Delete (Old_Name);
      end Rename;

      function To_JSON return GNATCOLL.JSON.JSON_Value is
         use Channel_List_Type;
         use GNATCOLL.JSON;
         Value     : constant JSON_Value := Create_Object;
         JSON_List : JSON_Array;
         Root      : constant JSON_Value := Create_Object;
      begin
         for C in Protected_List.Iterate loop
            Value.Set_Field (US.To_String (Key (C)),
                             Element (C).To_JSON);
            Append (JSON_List, Value);
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

      procedure Update (Key  : in Channel_Key;
                        Item : in AMI.Channel.Instance) is
      begin
         if Protected_List.Contains (Key) then
            Protected_List.Replace
              (Key      => Key,
               New_Item => Item);
         else
            raise Not_Found;
         end if;
      end Update;
   end Protected_Channel_List_Type;

   -----------
   --  "="  --
   -----------

   function "=" (Left : Instance; Right : Instance) return Boolean is
      use Channel_ID;
   begin
      if Left.Is_Null or Right.Is_Null then
         return Left.Is_Null and Right.Is_Null;
      else
         return Left.ID.Image = Left.ID.Image;
      end if;
   end "=";

   --------------------
   --  Add_Variable  --
   --------------------

   procedure Add_Variable (Channel :    out Instance;
                           Key     : in     US.Unbounded_String;
                           Value   : in     US.Unbounded_String) is
   begin
      if Channel.Variable.Contains (Key) then
         Channel.Variable.Replace (Key      => Key,
                                   New_Item => Value);
      else
         Channel.Variable.Insert (Key      => Key,
                                  New_Item => Value);
      end if;
   end Add_Variable;

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

      Bridged_Channel   : Channel_ID.Instance  := Channel_ID.Empty_Channel;
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

      --  Horribly inconsistant naming conflict fix. Sometimes
      --  the packet has a field named exten, and sometimes extension..
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
         Created_At            => Created_At,
         Variable              => Variable_Storage.Empty_Map);
   end Create;

   ------------------------
   --  To_Channel_State  --
   ------------------------

   function To_Channel_State (Item : in String) return Valid_State is
   begin
      return Valid_State'Val (Integer'Value (Item));
   end To_Channel_State;

   ------------------------
   --  To_Channel_State  --
   ------------------------

   function To_Channel_State (Item : in Integer) return Valid_State is
   begin
      return Valid_State'Val (Item);
   end To_Channel_State;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON (Channel : in Instance)
                     return GNATCOLL.JSON.JSON_Value is
      use Common;
      use GNATCOLL.JSON;
      use Ada.Characters.Handling;
      use Variable_Storage;

      JSON          : constant JSON_Value := Create_Object;
      Channel_JSON  : constant JSON_Value := Create_Object;
      Variable_JSON : constant JSON_Value := Create_Object;
   begin
      for Item in Channel.Variable.Iterate loop
         Variable_JSON.Set_Field (US.To_String (Key (Item)),
                                  US.To_String (Element (Item)));
      end loop;
      --  Append variables.
      Channel_JSON.Set_Field ("variable", Variable_JSON);

      Channel_JSON.Set_Field
        ("account_code", US.To_String (Channel.Account_Code));

      Channel_JSON.Set_Field
        ("name", Channel.ID.Image);

      Channel_JSON.Set_Field
        ("unique_id", Channel.Unique_ID.To_String);

      Channel_JSON.Set_Field
        ("bridged_unique_id", Channel.Bridged_Unique_ID.To_String);

      Channel_JSON.Set_Field
        ("state", To_Lower (Channel.State'Img));

      Channel_JSON.Set_Field
        ("caller_id_name", US.To_String (Channel.Caller_ID_Name));

      Channel_JSON.Set_Field
        ("caller_id_number", US.To_String (Channel.Caller_ID_Number));

      Channel_JSON.Set_Field ("extension", US.To_String (Channel.Extension));

      Channel_JSON.Set_Field ("context", US.To_String (Channel.Context));

      Channel_JSON.Set_Field ("bridged_with",
                              Channel.Bridged_With.Image);

      Channel_JSON.Set_Field ("priority", Channel.Priority'Img);

      Channel_JSON.Set_Field ("connected_line_number", US.To_String
                              (Channel.Connected_Line_Number));

      Channel_JSON.Set_Field ("connected_line_name",
                              Channel.Connected_Line_Name);

      Channel_JSON.Set_Field ("application", Channel.Application);

      Channel_JSON.Set_Field ("application_data", US.To_String
                              (Channel.Application_Data));

      Channel_JSON.Set_Field ("created_at", Common.Unix_Timestamp
                              (Channel.Created_At));

      JSON.Set_Field ("channel", Channel_JSON);
      JSON.Set_Field ("timestamp", Unix_Timestamp (Current_Time));

      return JSON;
   end To_JSON;

   ---------------
   --  To_String  --
   ---------------

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
