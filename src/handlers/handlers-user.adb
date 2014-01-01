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

with AWS.Session;

with Common,
     HTTP_Codes,
     Response,
     System_Message.Critical,
     System_Messages;

with Model.user;

package body Handlers.User is
   use AWS.Response;

   package HTTP renames HTTP_Codes;

   function Validate (Request : in AWS.Status.Data)
                      return AWS.Response.Data
   is
      use AWS.Session;
      use AWS.Status;

      Response_Object : Response.Object
        := Response.Factory (Request);
      Session_ID      : constant AWS.Session.Id
        := AWS.Status.Session (Request);
   begin
      declare
         Token : String renames Parameters (Request).Get ("token");
      begin
         Set (SID   => Session_ID,
              Key   => "token",
              Value => Token);
      exception
         when E : others =>
            System_Message.Critical.Response_Exception
              (Event           => E,
               Message         => "Validation failed",
               Response_Object => Response_Object);
            return Response_Object.Build;
      end;

      Response_Object.HTTP_Status_Code (HTTP.OK);

      return Response_Object.Build;

   end Validate;

   function Profile (Request : in AWS.Status.Data)
                      return AWS.Response.Data
   is
      Response_Object : Response.Object
        := Response.Factory (Request);
   begin
      raise Program_Error with "Handlers.User.Profile not implemented";

      Response_Object.HTTP_Status_Code (HTTP.OK);

      return Response_Object.Build;

   end Profile;

end Handlers.User;
