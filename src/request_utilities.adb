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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Black.HTTP,
     Black.Parameter.Vectors,
     Black.Response;

with GNATCOLL.JSON;

with Configuration,
     HTTP.Client,
     Protocol_Definitions,
     System_Messages;

package body Request_Utilities is
   package Config renames Configuration;

   ----------------
   --  Token_Of  --
   ----------------

   function Token_Of (Request : in Black.Request.Instance)
                      return Model.Token.Instance is
      use Black.Parameter.Vectors;
      Token : Unbounded_String := Null_Unbounded_String;
      C     : Cursor := Request.Parameters.First;
   begin
      while C /= No_Element loop
         if To_String (Element (C).Key) = "token" then
            Token := Element (C).Value;
         end if;
         Next (C);
      end loop;

      return Model.Token.Create (Value => To_String (Token));
   end Token_Of;

   ---------------
   --  User_Of  --
   ---------------

   function User_Of (Request : in Black.Request.Instance)
                     return Model.User.Instance is
      use Model,
          Protocol_Definitions;

      Context       : constant String := Package_Name & ".User_Of";

      Request_Token : Token.Instance renames Token_Of (Request => Request);
      In_JSON       : GNATCOLL.JSON.JSON_Value;

      URL           : constant String :=
        Config.Auth_Server & Separator &
        Token_Path & Separator &
        Request_Token.To_String;
   begin
      declare
         use type Black.HTTP.Statuses;
         Response : constant Black.Response.Class :=
                      HTTP.Client.Get (URL => URL);
      begin
         System_Messages.Debug
           (Message => URL & " status : HTTP " &
                       Black.HTTP.Statuses'Image (Response.Status),
            Context => Context);

         if Response.Status = Black.HTTP.OK then
            In_JSON := GNATCOLL.JSON.Read
              (Strm => Response.Content);

            return Model.User.Create (In_JSON);
         else
            return Model.User.No_User;
         end if;
      end;
   exception
      when others =>
         System_Messages.Error (Message => "User lookup failed! on URL " &
                                  URL,
                                Context => Context);
         return Model.User.No_User;
   end User_Of;

end Request_Utilities;
