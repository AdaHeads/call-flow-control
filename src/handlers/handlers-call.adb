-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
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

with Handlers.Call.Hangup,
     Handlers.Call.List,
     Handlers.Call.Originate,
     Handlers.Call.Park,
     Handlers.Call.Pickup,
     Handlers.Call.Queue,
     Handlers.Call.Transfer,
     Handlers.Not_Found,
     Namespaces;

package body Handlers.Call is
   procedure Handle (Stream  : in     GNAT.Sockets.Stream_Access;
                     Request : in     Black.Request.Instance) is
   begin
      case Namespaces.Call_Resource (Request) is
         when Namespaces.Not_Found =>
            Handlers.Not_Found.Handle (Stream  => Stream,
                                       Request => Request);
         when Namespaces.Hangup =>
            Handlers.Call.Hangup.Handle (Stream  => Stream,
                                       Request => Request);
         when Namespaces.List =>
            Handlers.Call.List.Handle (Stream  => Stream,
                                       Request => Request);
         when Namespaces.Originate =>
            Handlers.Call.Originate.Handle (Stream  => Stream,
                                            Request => Request);
         when Namespaces.Park =>
            Handlers.Call.Park.Handle (Stream  => Stream,
                                       Request => Request);
         when Namespaces.Pickup =>
            Handlers.Call.Pickup.Handle (Stream  => Stream,
                                         Request => Request);
         when Namespaces.Queue =>
            Handlers.Call.Queue.Handle (Stream  => Stream,
                                        Request => Request);
         when Namespaces.Transfer =>
            Handlers.Call.Transfer.Handle (Stream  => Stream,
                                           Request => Request);
      end case;
   end Handle;
end Handlers.Call;
