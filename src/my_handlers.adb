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
with Call_Queue;
with Contact;
with Contact_Attributes;
with Contact_Full;
with My_Configuration;
with Organization;
with Organization_Contacts;
with Organization_Contacts_Attributes;
with Organization_Contacts_Full;
with Yolk.Not_Found;

package body My_Handlers is

   -----------
   --  Set  --
   -----------

   procedure Set
     (RH : out AWS.Services.Dispatchers.URI.Handler)
   is
      use AWS.Dispatchers.Callback;

      package My renames My_Configuration;
   begin
      -----------------------------------------
      --  Unknown Resource (404) Dispatcher  --
      -----------------------------------------

      AWS.Services.Dispatchers.URI.Register_Default_Callback
        (Dispatcher => RH,
         Action     => Create (Callback => Yolk.Not_Found.Generate'Access));
      --  This dispatcher is called if the requested resource doesn't match any
      --  of the other dispatchers.
      --  It returns a generic 404 HTML page. The template for this 404 can be
      --  found in templates/system.
      --  Another option is of course to use this default callback for your
      --  main content, so if unknown resources are called, then the main
      --  content of the website is used. I personally prefer giving back 404's
      --  if unknown content is requested by a client.

      ------------------
      --  Dispatchers --
      ------------------

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => My.Config.Get (My.Handler_Get_Call),
         Action     => Create
           (Callback => Call_Queue.Get_Call'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => My.Config.Get (My.Handler_Get_Contact),
         Action     => Contact.Callback);

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => My.Config.Get (My.Handler_Get_Contact_Attributes),
         Action     => Contact_Attributes.Callback);

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => My.Config.Get (My.Handler_Get_Contact_Full),
         Action     => Contact_Full.Callback);

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => My.Config.Get (My.Handler_Get_Organization_Contacts),
         Action     => Organization_Contacts.Callback);

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => My.Config.Get
           (My.Handler_Get_Organization_Contacts_Attributes),
         Action     => Organization_Contacts_Attributes.Callback);

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => My.Config.Get
           (My.Handler_Get_Organization_Contacts_Full),
         Action     => Organization_Contacts_Full.Callback);

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => My.Config.Get (My.Handler_Get_Organization),
         Action     => Organization.Callback);

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => My.Config.Get (My.Handler_Get_Queue),
         Action     => Create
           (Callback => Call_Queue.Get'Access));

      AWS.Services.Dispatchers.URI.Register
        (Dispatcher => RH,
         URI        => My.Config.Get (My.Handler_Get_Queue_Length),
         Action     => Create
           (Callback => Call_Queue.Get_Queue_Length'Access));
   end Set;

end My_Handlers;
