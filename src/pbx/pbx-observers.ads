with ESL.Packet_Keys;
with ESL.Packet;

package PBX.Observers is

   use ESL.Packet_Keys;

   type Instance (Observing_Event : Inbound_Events) is
     abstract tagged null record;

   procedure Notify (Object : in Instance;
                     Packet : in ESL.Packet.Instance) is abstract;

   function "=" (Left, Right : in Instance) return Boolean is abstract;
   --  The equals operator needs to be overwritten in order to make sure
   --  that the same observer does not register in multiple times.

end PBX.Observers;
