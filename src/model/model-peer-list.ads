-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2013-, AdaHeads K/S                     --
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

package Model.Peer.List is
   Package_Name : constant String := "Model.Peer.List";

   type Instance is tagged private;

   type Reference is access all Instance;

   Not_Found : exception;

   procedure Set_Singleton (Object : in Instance);
   function Get_Singleton return Reference;
   --  Returns the internal singleton object for the server.
   --  An easy ad-hoc way of storing the data locally in the package while
   --  determining where it should really be located.

   function Get (Object   : in Instance;
                 Identity : in Peer.Identification) return Peer.Instance;

   procedure Put (Object   :    out Instance;
                  New_Peer : in     Peer.Instance);

   procedure Register (Object   : in out Instance;
                       Identity : in     Peer.Identification;
                       Contact  : in     String;
                       Expiry   : in     Natural);

   function To_JSON (Object : in Instance) return JSON_Value;
   --  Gives back the JSON representation of the list.

   procedure Unregister (Object   : in out Instance;
                         Identity : in     Peer.Identification);

private
   type Instance is tagged
      record
         Peer_Map : Peer_Maps := Peer.Peer_Storage.Empty_Map;
      end record;

end Model.Peer.List;
