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
     Black.MIME_Types;

package body Response.Templates is

   procedure Add_CORS_Headers
     (Request  : in     Black.Request.Class;
      Response : in out Black.Response.Class);
   --  If the client sends the Origin: header, add these two CORS headers:
   --    Access-Control-Allow-Origin
   --    Access-Control-Allow-Credentials
   --  where the first one should contain the value of the given
   --  Origin : header and the second a Boolean True. This should be enough
   --  to enable very simple CORS support.

   procedure Add_CORS_Headers
     (Request  : in     Black.Request.Class;
      Response : in out Black.Response.Class)
   is
      use Black.HTTP, Black.Response.Access_Control;
   begin
      if Request.Has_Origin then
         Allow_Origin      (Response, Request.Origin);
         Allow_Credentials (Response);
         Allow_Headers     (Response, (Get | Post => True,
                                       others     => False));
         Max_Age           (Response, 86_400.0);
      end if;
   end Add_CORS_Headers;

   ----------------------
   --  Bad_Parameters  --
   ----------------------

   function Bad_Parameters (Request       : in Black.Request.Instance;
                            Response_Body : in JSON_Value := Create_Object)
                           return Black.Response.Class is
      Content : constant JSON_Value := Response_Body;
   begin
      Content.Set_Field (Status_Text, Bad_Parameters_Reponse_Text);

      declare
         Response : Black.Response.Class :=
           Black.Response.Bad_Request
             (Content_Type => Black.MIME_Types.Application.JSON,
              Data         => Response_Body.Write);
      begin
         Add_CORS_Headers (Request  => Request,
                           Response => Response);
         return Response;
      end;
   end Bad_Parameters;

   ----------------------
   --  Not_Authorized  --
   ----------------------

   function Not_Authorized (Request : in Black.Request.Instance)
                           return Black.Response.Class is
      Response_Body : constant JSON_Value := Create_Object;
   begin
      Response_Body.Set_Field (Status_Text, Not_Authorized_Reponse_Text);

      return Response : Black.Response.Class :=
        Black.Response.Unauthorized
          (Content_Type => Black.MIME_Types.Application.JSON,
           Data         => Response_Body.Write)
      do
         Add_CORS_Headers (Request  => Request,
                           Response => Response);
      end return;
   end Not_Authorized;

   -----------------
   --  Not_Found  --
   -----------------

   function Not_Found (Request       : in Black.Request.Instance;
                       Response_Body : in JSON_Value := Create_Object)
                      return Black.Response.Class is
   begin
      Response_Body.Set_Field (Status_Text, Not_Found_Reponse_Text);

      return Response : Black.Response.Class :=
        Black.Response.Not_Found
          (Content_Type => Black.MIME_Types.Application.JSON,
           Data         => Response_Body.Write)
      do
         Add_CORS_Headers (Request  => Request,
                           Response => Response);
      end return;
   end Not_Found;

   ----------
   --  OK  --
   ----------

   function OK (Request       : in Black.Request.Instance;
                Response_Body : in JSON_Value := Create_Object)
               return Black.Response.Class is
      Content : constant JSON_Value := Response_Body;
   begin
      Content.Set_Field (Status_Text, OK_Reponse_Text);

      return Response : Black.Response.Class :=
        Black.Response.OK (Content_Type => Black.MIME_Types.Application.JSON,
                           Data         => Response_Body.Write)
      do
         Add_CORS_Headers (Request  => Request,
                           Response => Response);
      end return;
   end OK;

   --------------------
   --  Server_Error  --
   --------------------

   function Server_Error (Request       : in Black.Request.Instance;
                          Response_Body : in JSON_Value := Create_Object)
                         return Black.Response.Class is
      pragma Unreferenced (Request);
      --  Why don't we include something from the request in the response?

      Content : constant JSON_Value := Response_Body;
   begin
      Content.Set_Field (Status_Text, Server_Error_Reponse_Text);

      return Black.Response.Server_Error
               (Content_Type => Black.MIME_Types.Application.JSON,
                Data         => Content.Write);
   end Server_Error;

end Response.Templates;
