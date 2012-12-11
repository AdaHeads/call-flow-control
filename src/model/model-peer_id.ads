with Ada.Strings.Unbounded;

package Model.Peer_ID is
   use Ada.Strings.Unbounded;

   Invalid_ID : exception;

   type Channel_Type is (Unknown, Agent, Console,
                               H323, IAX, IAX2, Local,
                               MGCP, MISDN, Modem, NBS,
                               Phone, SIP, Skinny, Gtalk,
                               VPB, ZAP);

   --  TODO: Add special cases such as MulticastRTP, VOFR

   type Peer_ID_Type is tagged record
      Kind     : Channel_Type     := Unknown;
      Peername : Unbounded_String := Null_Unbounded_String;
   end record;

   function Create (Item : in String) return Peer_ID_Type;
   --  Constructor which expect the format <Channel_type>/<Peername>.

   function Create (Channel_Kind : in String;
                    Peername     : in String) return Peer_ID_Type;
   --  Constructor which take in each part of the ID in seperately.

   function To_String (Peer_ID : in Peer_ID_Type) return String;

   function "<" (Left  : in Peer_ID_Type;
                 Right : in Peer_ID_Type) return Boolean;

   function "=" (Left  : in Peer_ID_Type;
                 Right : in Peer_ID_Type) return Boolean;

   Null_Peer_ID : constant Peer_ID_Type := (Unknown, Null_Unbounded_String);
end Model.Peer_ID;
