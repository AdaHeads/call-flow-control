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

with GNATCOLL.JSON;

with Response.Templates,
     View,
     Model.Token.List,
     Model.Peer.List;

package body Handlers.Debug is
   use View;

   --------------------
   --  Channel_List  --
   --------------------

   function Channel_List (Request : in AWS.Status.Data)
                          return AWS.Response.Data is
      use GNATCOLL.JSON;
      use HTTP_Codes;
   begin
      return Response.Templates.Server_Error
        (Request       => Request,
         Response_Body => Description ("Not implemented"));
   end Channel_List;

   ---------------
   --  Contact  --
   ---------------

   function Contact
     (Request : in AWS.Status.Data)
      return AWS.Response.Data is
      use GNATCOLL.JSON;
   begin
      return Response.Templates.Server_Error
        (Request       => Request,
         Response_Body => Description ("Not implemented"));
--             (Model.Contact.Fetch (Reception => 1,
--                                   Contact   => 1).Image));
   end Contact;

   ----------------------
   --  Dummy_Response  --
   ----------------------

   function Dummy_Response
     (Request : in AWS.Status.Data)
      return AWS.Response.Data is
   begin
      return Response.Templates.OK (Request);
   end Dummy_Response;

   --------------------
   --  Dummy_Tokens  --
   --------------------

   function Dummy_Tokens (Request : in AWS.Status.Data)
                       return AWS.Response.Data is
   begin
      return Response.Templates.OK
        (Request       => Request,
         Response_Body => Model.Token.List.Get_Singleton.To_JSON);
   end Dummy_Tokens;

   -----------------
   --  Peer_List  --
   -----------------

   function Peer_List (Request : in AWS.Status.Data)
                       return AWS.Response.Data is
   begin
      return Response.Templates.OK
        (Request       => Request,
         Response_Body => Model.Peer.List.Get_Singleton.To_JSON);
   end Peer_List;

end Handlers.Debug;
