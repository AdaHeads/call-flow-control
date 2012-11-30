package body Model.Channel is
   function Description (Channel : in Channel_State_Type) return String is
   begin
      case Channel is
      when Hung_Up =>
         return "Other end has hungup";
      when Local_Ring =>
         return "Local ring";
      when Ringing =>
         return "Remote end is ringing";
      when Answered =>
         return "Remote end has answered";
      when Busy =>
         return "Remote end is busy";
      when Off_Hook =>
         return "Make it go off hook";
      when Is_Off_Hook =>
         return "Line is off hook";
      when Congestion =>
         return "Congestion (circuits busy)";
      when others =>
         return "Unkown channel state";
      end case;
   end Description;

   function To_Channel_State (Item : in String) return Channel_State_Type is
   begin
      return To_Channel_State (Integer'Value (Item));
   end To_Channel_State;

   function To_Channel_State (Item : in Integer) return Channel_State_Type is
   begin
      return Channel_State_Type'Val (Item);
   end To_Channel_State;

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
