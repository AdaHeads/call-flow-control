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

--  This package contains a basic set of "template" functions for creating
--  client events - ready to be sent.

with GNATCOLL.JSON;
with Model.Call,
     Model.Peer;

package Client_Notification is
   use GNATCOLL.JSON;

   type Event_Names is (Call_Offer,
                        Call_Lock,
                        Call_Unlock,
                        Call_Pickup,
                        Call_State,
                        Call_Hangup,
                        Call_Park,
                        Call_Unpark,
                        Call_Transfer,
                        Call_Bridge,
                        Queue_Join,
                        Queue_Leave,
                        Peer_State,
                        Originate_Failed,
                        Originate_Success);

   function Image (Item : in Event_Names) return String;
   --  Conversion function that makes sure that Event_Names are formatted
   --  correctly when serialzed.

   type Instance (Name : Event_Names) is tagged private;

   function JSON_Root (Event : in Instance'Class) return JSON_Value;

   function To_JSON (Event : in Instance'Class) return JSON_Value;

   function Image (Event : in Instance'Class) return String;

   procedure JSON_Append (Node  : in JSON_Value;
                          Key   : in String;
                          Value : in JSON_Value);
   --  Appends additional JSON objects to the body.

   function Peer_State (P : in Model.Peer.Instance) return Instance;
   function Call_Offer (C : in Model.Call.Instance) return Instance;
   function Call_Transfer (C : in Model.Call.Instance) return Instance;
   function Call_Lock (C : in Model.Call.Instance) return Instance;
   function Call_State (C : in Model.Call.Instance) return Instance;
   function Call_Unlock (C : in Model.Call.Instance) return Instance;
   function Pickup (C : in Model.Call.Instance) return Instance;
   function Hangup (C : in Model.Call.Instance) return Instance;
   function Park (C : in Model.Call.Instance) return Instance;
   function Unpark (C : in Model.Call.Instance) return Instance;
   function Join (C : in Model.Call.Instance) return Instance;
   function Leave (C : in Model.Call.Instance) return Instance;

private
   Root_Field      : constant String := "notification";
   Event_Field     : constant String := "event";
   Timestamp_Field : constant String := "timestamp";

   type Instance (Name : Event_Names) is tagged
      record
         Root : JSON_Value;
      end record;

end Client_Notification;
