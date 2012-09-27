-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               Event_Parser                                --
--                                                                           --
--                                  SPEC                                     --
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

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;

--  This package can parse, the Events that comes from Asterisk.
package Event_Parser is
   use Ada.Strings.Unbounded;

   type Event_Keywords is
     (Event,
      Response,
      Channel,
      Channel1,
      Channel2,
      CallerID,
      CallerIDName,
      Queue,
      Position,
      Count,
      Uniqueid,
      Uniqueid1,
      Uniqueid2,
      State,
      Cause,
      Source,
      Destination,
      SrcUniqueID,
      DestUniqueID,
      Extension,
      Priority,
      Application,
      AppData,
      Oldname,
      Newname,
      Shutdown,
      Restart,
      Peer,
      PeerStatus,
      Time,
      Message,
      Exten,
      Address,
      Port,
      Privilege,
      ChannelType);

   function Hash_Function (Key : in Event_Keywords)
                           return Ada.Containers.Hash_Type;
   function Hash_Equivalent_Keys (Left, Right : in Event_Keywords)
                                  return Boolean;

   package Event_List_Type is new Ada.Containers.Hashed_Maps
     (Key_Type => Event_Keywords,
      Element_Type => Unbounded_String,
      Hash => Hash_Function,
      Equivalent_Keys => Hash_Equivalent_Keys);

   --  Takes a line of text, with key-value pairs structured:
   --  Key: Value<CRLF>
   function Parse (Event_Text : in Unbounded_String)
                   return Event_List_Type.Map;

   function Try_Get (Events     : in     Event_List_Type.Map;
                     Field_Name : in     Event_Keywords;
                     Value      :    out Unbounded_String) return Boolean;
end Event_Parser;
