-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               My_Handlers                                 --
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

--  Application specific resource handlers.

with AWS.Services.Dispatchers.URI;

package My_Handlers is

   procedure Set
     (RH : out AWS.Services.Dispatchers.URI.Handler);
   --  Setup content dispatchers for the server. Basically this initializes the
   --  RH object declared in yolk_demo.adb. The handlers registered here are
   --  specific to this application. Generic content handlers, such as 404
   --  errors, images and similar, are registered in the core Yolk.Handlers
   --  package.

end My_Handlers;
