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

with AWS.Client,
     AWS.Messages,
     AWS.Response;

with System_Messages,
     Util.Image,
     Protocol_Definitions;

package body Model.Contact.Utilities is
   use Model;
   use AWS.Messages;

   ----------------
   --  Retrieve  --
   ----------------

   function Retrieve (Reception : in Reception_Identifier;
                      Contact   : in Contact_Identifier;
                      Token     : in String;
                      From      : in String) return Instance is
      use Protocol_Definitions;

      Context  : constant String := Package_Name & ".Retrieve";
      In_JSON  : GNATCOLL.JSON.JSON_Value;
      Response : AWS.Response.Data;
      URL      : constant String := From &
        Contact_Path    & Separator & Util.Image.Image (Contact) &
        Reception_Path  & Separator & Util.Image.Image (Reception) &
        Token_Parameter & Token;
   begin

      Response := AWS.Client.Get (URL);

      System_Messages.Debug
        (Message => URL & " status : HTTP " &
           AWS.Response.Status_Code (Response)'Img,
         Context => Context);

      if AWS.Response.Status_Code (Response) = S200 then
         In_JSON := GNATCOLL.JSON.Read
           (Strm => AWS.Response.Message_Body (Response));

         return Model.Contact.Create_From_JSON (In_JSON);
      else
         return Model.Contact.No_Contact;
      end if;

   exception
      when others =>
         System_Messages.Error (Message => "Contact lookup failed!",
                                Context => Context);
         return Model.Contact.No_Contact;
   end Retrieve;

end Model.Contact.Utilities;
