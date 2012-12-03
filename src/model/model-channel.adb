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
      Channel_JSON.Set_Field ("ID", To_String (Channel.ID));
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
        "ID => "           & Channel.ID.To_String & ", " &
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
