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

package Model.Channel_ID is
   use Ada.Strings.Unbounded;

   type Technology is (Unknown, SIP);

   --  TODO: Make aggregate of Peer_ID and sequence
   type Channel_ID_Type is tagged record
      Kind     : Technology;
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
