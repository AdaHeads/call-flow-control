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

package AMI.Channel_ID is
   type Technologies    is (None, SIP, ZAP);
   --  These are the various form of technolgies used within Asterisk.
   --  The list is far from complete.

   type Transitions     is (None, Bridge, Async_Goto, Parked);
   --  These are various forms of transition states that a channel can take.
   --  It will also reveal if a channel is temporary or not.

   type Volatility      is (None, Zombie, Masq);
   --  Determines whether or not the channel is volatile, and the type of
   --  volatility.

   type Peer_Name       is new Ada.Strings.Unbounded.Unbounded_String;
   type Sequence_Number is mod 16 ** 8;

   function Image (Item : in Peer_Name) return String;

   function Image (Item : in Sequence_Number) return String;

   --  function Value (Item : in String) return Sequence_Number;

   Invalid_ID : exception;

   type Instance (Is_Null : Boolean) is tagged record
      case Is_Null is
         when False =>
            Transition : Transitions;
            Technology : Technologies;
            Peer       : Peer_Name;
            Sequence   : Sequence_Number;
            Volatile   : Volatility;
         when True =>
            null;
      end case;
   end record;

   function Value (Item : in String) return Instance;

   function Create (Item : in String) return Instance renames Value;
   pragma Obsolescent (Create);

   function Image (Item : in Instance) return String;

   function Temporary (Item : in Instance) return Boolean;

   function Is_Local (Item : in Instance) return Boolean;
   --  Reveals whether or not the channel belongs to a local peer.

   function "<" (Left  : in Instance;
                 Right : in Instance) return Boolean;

   function "=" (Left  : in Instance;
                 Right : in Instance) return Boolean;

   function Validate (Item : in String) return Boolean;
   --  Non-exception-raising way of checking whether a string can be
   --  converted into a Channel_ID

   Null_Channel_ID : constant Instance;
   Empty_Channel   : constant Instance;
private

   Null_Channel_ID : constant Instance := (Is_Null => True);
   Empty_Channel   : constant Instance :=
                       (Is_Null    => False,
                        Transition => None,
                        Technology => None,
                        Peer       => Peer_Name
                            (Ada.Strings.Unbounded.Null_Unbounded_String),
                        Sequence   => 16#00000000#,
                        Volatile   => None);

end AMI.Channel_ID;
