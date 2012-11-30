with Ada.Strings.Unbounded;

with Model.Call_ID;
with Model.Channel_ID;

package Model.Channel is
   use Model.Call_ID;
   use Model.Channel_ID;

   use Ada.Strings.Unbounded;
   type Channel_State_Type is (Unknown, Bad_Extention, Hung_Up, Local_Ring,
                               Ringing, Answered, Busy, Off_Hook,
                               Is_Off_Hook, Congestion);
   for Channel_State_Type use (Unknown       => -1,
                               Bad_Extention => 0,
                               Hung_Up       => 1,
                               Local_Ring    => 2,
                               Ringing       => 3,
                               Answered      => 4,
                               Busy          => 5,
                               Off_Hook      => 6,
                               Is_Off_Hook   => 7,
                               Congestion    => 8);

   type Channel_Type is tagged record
      ID           : Channel_ID_Type;
      Call_ID      : Call_ID_Type;
      State        : Channel_State_Type;
      Description  : Unbounded_String;
      CallerIDNum  : Unbounded_String;
      CallerIDName : Unbounded_String;
      AccountCode  : Unbounded_String;
      Extension    : Unbounded_String;
      Context      : Unbounded_String;
   end record;

   Null_Channel : constant Channel_Type;

   function Description (Channel : in Channel_State_Type) return String;
   --  Returns a debug- and human friendly description of what the channel
   --  state represents.

   function To_Channel_State (Item : in String) return Channel_State_Type;
   function To_Channel_State (Item : in Integer) return Channel_State_Type;
   function To_String (Channel : in Channel_Type) return String;
   --  Conversion functions.
private
   Null_Channel : constant Channel_Type :=
                    (ID           => Null_Channel_ID,
                     Call_ID      => Null_Call_ID,
                     State        => Unknown,
                     Description  => Null_Unbounded_String,
                     CallerIDName => Null_Unbounded_String,
                     CallerIDNum  => Null_Unbounded_String,
                     AccountCode  => Null_Unbounded_String,
                     Extension    => Null_Unbounded_String,
                     Context      => Null_Unbounded_String);

end Model.Channel;
