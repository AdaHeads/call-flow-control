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

with AWS.Response,
     AWS.Status,
     GNATCOLL.JSON;

with Common,
     Handlers.OpenID,
     HTTP_Codes,
     MIME_Types,
     View;

package body Handlers.Users.Log_Out is
   function Generate_Response (Request : in AWS.Status.Data)
                              return AWS.Response.Data;

   function Callback return AWS.Dispatchers.Callback.Handler is
   begin
      return AWS.Dispatchers.Callback.Create (Generate_Response'Access);
   end Callback;

   function Generate_Response (Request : in AWS.Status.Data)
                              return AWS.Response.Data is
      use GNATCOLL.JSON;
      use Common;

      function Parameters_Okay return Boolean;
      function Bad_Parameters return AWS.Response.Data;

      function Bad_Parameters return AWS.Response.Data is
         Data : JSON_Value;
      begin
         Data := Create_Object;

         Data.Set_Field (Field_Name => View.Status,
                         Field      => "bad parameters");

         return AWS.Response.Build
                  (Content_Type => MIME_Types.JSON,
                   Message_Body => To_String (To_JSON_String (Data)),
                   Status_Code  => HTTP_Codes.Bad_Request);
      end Bad_Parameters;

      function Parameters_Okay return Boolean is
      begin
         return AWS.Status.Parameters (Request).Count = 0;
      exception
         when others =>
            return False;
      end Parameters_Okay;

   begin
      if Parameters_Okay then
         return Handlers.OpenID.Log_Out (Request);
      else
         return Bad_Parameters;
      end if;
   end Generate_Response;

end Handlers.Users.Log_Out;
