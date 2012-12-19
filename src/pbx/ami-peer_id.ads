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

with Ada.Strings.Unbounded;

package AMI.Peer_ID is
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
end AMI.Peer_ID;
