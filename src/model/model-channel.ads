with Ada.Strings.Unbounded;

with GNATCOLL.JSON;

with Model.Call_ID;
with Model.Channel_ID;

package Model.Channel is
   use Model.Call_ID;
   use Model.Channel_ID;

   use Ada.Strings.Unbounded;

   type Channel_State_Type is (Unknown, Down, Reserved, Off_Hook,
                               Dialed, Ringing, Receiver_Ringing, Up, Busy);

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

   function To_JSON (Channel : in Channel_Type)
                     return GNATCOLL.JSON.JSON_Value;

   Null_Channel : constant Channel_Type;

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

   Channel_State_Map : array (-1 .. 7) of Channel_State_Type :=
                         (-1  => Unknown,
                          0   => Down,
                          1   => Reserved,
                          2   => Off_Hook,
                          3   => Dialed,
                          4   => Ringing,
                          5   => Receiver_Ringing,
                          6   => Up,
                          7   => Busy);

end Model.Channel;
