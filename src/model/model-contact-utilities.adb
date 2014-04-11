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

with Black.HTTP,
     Black.Response;

with HTTP.Client,
     Protocol_Definitions,
     System_Messages,
     Util.Image;

package body Model.Contact.Utilities is

   ----------------
   --  Retrieve  --
   ----------------

   function Retrieve (Reception : in Reception_Identifier;
                      Contact   : in Contact_Identifier;
                      Token     : in String;
                      From      : in String) return Instance is
      Context  : constant String := Package_Name & ".Retrieve";

      In_JSON  : GNATCOLL.JSON.JSON_Value;
   begin
      declare
         use Protocol_Definitions;
         use type Black.HTTP.Statuses;
         URL : constant String :=
           From &
           Contact_Path    & Separator & Util.Image.Image (Contact) &
           Reception_Path  & Separator & Util.Image.Image (Reception) &
           Token_Parameter & Token;
         Response : constant Black.Response.Class := HTTP.Client.Get (URL);
      begin
         System_Messages.Debug
           (Message => URL & " status : HTTP " &
                       Black.HTTP.Statuses'Image (Response.Status),
            Context => Context);

         if Response.Status = Black.HTTP.OK then
            In_JSON := GNATCOLL.JSON.Read (Strm => Response.Content);

            return Model.Contact.Create_From_JSON (In_JSON);
         else
            return Model.Contact.No_Contact;
         end if;
      end;
   exception
      when others =>
         System_Messages.Error (Message => "Contact lookup failed!",
                                Context => Context);
         return Model.Contact.No_Contact;
   end Retrieve;

end Model.Contact.Utilities;
