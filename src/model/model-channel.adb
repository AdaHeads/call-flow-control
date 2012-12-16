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
with Common;

package body Model.Channel is

   function To_Channel_State (Item : in String) return Channel_State_Type is
   begin
      return Channel_State_Map (Integer'Value (Item));
   end To_Channel_State;

   function To_Channel_State (Item : in Integer) return Channel_State_Type is
   begin
      return Channel_State_Type'Val (Item);
   end To_Channel_State;

   function To_JSON (Channel : in Channel_Type)
                     return GNATCOLL.JSON.JSON_Value is
      use Common;
      use GNATCOLL.JSON;
      use Ada.Characters.Handling;

      JSON         : constant JSON_Value := Create_Object;
      Channel_JSON : constant JSON_Value := Create_Object;
   begin
      Channel_JSON.Set_Field ("ID", Channel.ID.Image);
      Channel_JSON.Set_Field ("Call_ID", Channel.Call_ID.To_String);
      Channel_JSON.Set_Field ("State", To_Lower (Channel.State'Img));

      Channel_JSON.Set_Field ("Description", To_String (Channel.Description));
      Channel_JSON.Set_Field
        ("CallerIDName", To_String (Channel.CallerIDName));
      Channel_JSON.Set_Field ("CallerIDNum", To_String (Channel.CallerIDNum));
      Channel_JSON.Set_Field ("AccountCode", To_String (Channel.AccountCode));
      Channel_JSON.Set_Field ("Extension", To_String (Channel.Extension));
      Channel_JSON.Set_Field ("Context", To_String (Channel.Context));
      JSON.Set_Field ("channel", Channel_JSON);
      JSON.Set_Field ("timestamp", Unix_Timestamp (Current_Time));

      return JSON;
   end To_JSON;

   function To_String (Channel : in Channel_Type) return String is
   begin
      return
        "ID => "           & Channel.ID.Image & ", " &
        "Call_ID => "      & Channel.Call_ID.To_String & ", " &
        "State => "        & Channel.State'Img & ", " &
        "Description => "  & To_String (Channel.Description) & ", " &
        "CallerIDNum => "  & To_String (Channel.CallerIDNum) & ", " &
        "CallerIDName => " & To_String (Channel.CallerIDName) & ", " &
        "AccountCode => "  & To_String (Channel.AccountCode) & ", " &
        "Extension => "    & To_String (Channel.Extension) & ", " &
        "Context => "      & To_String (Channel.Context);
   end To_String;
end Model.Channel;
