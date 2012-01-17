-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                               My_Handlers                                 --
--                                                                           --
--                                  BODY                                     --
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

with AWS.Dispatchers.Callback;
with My_Configuration;
with Request;
with Yolk.Not_Found;

package body My_Handlers is

   -----------
   --  Set  --
   -----------

   procedure Set
     (RH : out AWS.Services.Dispatchers.URI.Handler)
   is
      use AWS.Dispatchers.Callback;
      use Yolk;

      package My renames My_Configuration;
   begin
      -----------------------------------------
      --  Unknown Resource (404) Dispatcher  --
      -----------------------------------------

      AWS.Services.Dispatchers.URI.Register_Default_Callback
        (Dispatcher => RH,
         Action     => Create (Callback => Not_Found.Generate'Access));
      --  This dispatcher is called if the requested resource doesn't match any
      --  of the other dispatchers.
      --  It returns a generic 404 HTML page. The template for this 404 can be
      --  found in templates/system.
      --  Another option is of course to use this default callback for your
      --  main content, so if unknown resources are called, then the main
      --  content of the website is used. I personally prefer giving back 404's
      --  if unknown content is requested by a client.

      ---------------------------------------
      --  Dispatchers for known resources  --
      ---------------------------------------

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => My.Config.Get (My.Handler_Get_Company),
         Action     => Create
           (Callback => Request.Company'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => My.Config.Get (My.Handler_Get_Persons),
         Action     => Create
           (Callback => Request.Persons'Access));
   end Set;

end My_Handlers;
