-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Request                                   --
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

with AWS.Status;
with AWS.Response;

package Request is

   function Call
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Get a call JSON for the longest waiting call in the queue.

   function Contact_Attributes
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Get a Contact_Attributes JSON.

   function Contact_Full
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Get a Contact JSON with Attributes.

   function Org_Contacts
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Get the Contacts JSON.

   function Org_Contacts_Attributes
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Get the Contacts_Attributes JSON.

   function Org_Contacts_Full
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Get the Contacts JSON with Attributes.

   function Organization
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Get the Organization JSON.

   function Queue
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Get the call queue JSON.

   function Queue_Length
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Get the call queue length JSON.

end Request;
