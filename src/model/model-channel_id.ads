with Ada.Strings.Unbounded;

package Model.Channel_ID is
   use Ada.Strings.Unbounded;

   type Channel_Class_Type is (Unknown, SIP);

   --  TODO: Make aggregate of Peer_ID and sequence
   type Channel_ID_Type is tagged record
      Class     : Channel_Class_Type;
      Peername  : Unbounded_String;
      Sequence  : String (1 .. 8);
   end record;

   function Create (Item : in String) return Channel_ID_Type;
   --  Constructor.

   function To_String (Channel_ID : in Channel_ID_Type) return String;

   function "<" (Left  : in Channel_ID_Type;
                 Right : in Channel_ID_Type) return Boolean;

   function "=" (Left  : in Channel_ID_Type;
                 Right : in Channel_ID_Type) return Boolean;

   Null_Channel_ID : constant Channel_ID_Type := (Unknown,
                                                  Null_Unbounded_String,
                                                  "FFFFFFFF");
end Model.Channel_ID;
