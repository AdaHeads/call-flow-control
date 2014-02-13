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

with AWS.Messages;
with AWS.Response.Set;
with HTTP_Codes;
with MIME_Types;

package body Response.Templates is

   procedure Add_CORS_Headers
     (Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data);
   --  If the client sends the Origin: header, add these two CORS headers:
   --    Access-Control-Allow-Origin
   --    Access-Control-Allow-Credentials
   --  where the first one should contain the value of the given
   --  Origin : header and the second a Boolean True. This should be enough
   --  to enable very simple CORS support in Alice.

   procedure Add_CORS_Headers
     (Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data)
   is
      use AWS.Messages;
      use AWS.Response;
      use AWS.Status;

      Origin_Host : constant String := Origin (Request);
   begin
      if Origin_Host'Length > 0 then
         Set.Add_Header (D     => Response,
                         Name  => Access_Control_Allow_Origin_Token,
                         Value => Origin_Host);

         Set.Add_Header (D     => Response,
                         Name  => Access_Control_Allow_Credentials_Token,
                         Value => "true");

         Set.Add_Header (D     => Response,
                         Name  => Access_Control_Allow_Headers_Token,
                         Value => "POST, GET");

         Set.Add_Header (D     => Response,
                         Name  => Access_Control_Max_Age_Token,
                         Value => "86400");
      end if;
   end Add_CORS_Headers;

   ----------------------
   --  Bad_Parameters  --
   ----------------------

   function Bad_Parameters (Request : in AWS.Status.Data;
                            Response_Body : in JSON_Value := Create_Object)
                            return AWS.Response.Data is
   begin

      Response_Body.Set_Field (Status_Text, Bad_Parameters_Reponse_Text);

      return Response : AWS.Response.Data do
         Response := AWS.Response.Build
           (Content_Type  => MIME_Types.JSON,
            Message_Body  => Response_Body.Write,
            Status_Code   => HTTP_Codes.Bad_Request,
            Cache_Control => AWS.Messages.No_Cache,
            Encoding      => AWS.Status.Preferred_Coding (Request));
         Add_CORS_Headers (Request  => Request,
                           Response => Response);
      end return;

   end Bad_Parameters;

   ----------------------
   --  Not_Authorized  --
   ----------------------

   function Not_Authorized (Request       : in AWS.Status.Data)
                            return AWS.Response.Data is
      Response_Body : constant JSON_Value := Create_Object;
   begin

      Response_Body.Set_Field (Status_Text, Not_Authorized_Reponse_Text);

      return Response : AWS.Response.Data do
         Response := AWS.Response.Build
           (Content_Type  => MIME_Types.JSON,
            Message_Body  => Response_Body.Write,
            Status_Code   => HTTP_Codes.Unauthorized,
            Cache_Control => AWS.Messages.No_Cache,
            Encoding      => AWS.Status.Preferred_Coding (Request));
         Add_CORS_Headers (Request  => Request,
                           Response => Response);
      end return;
   end Not_Authorized;

   -----------------
   --  Not_Found  --
   -----------------

   function Not_Found (Request       : in AWS.Status.Data;
                       Response_Body : in JSON_Value := Create_Object)
                       return AWS.Response.Data is
   begin
      Response_Body.Set_Field (Status_Text, Not_Found_Reponse_Text);

      return Response : AWS.Response.Data do
         Response := AWS.Response.Build
           (Content_Type  => MIME_Types.JSON,
            Message_Body  => Response_Body.Write,
            Status_Code   => HTTP_Codes.Not_Found,
            Cache_Control => AWS.Messages.No_Cache,
            Encoding      => AWS.Status.Preferred_Coding (Request));
         Add_CORS_Headers (Request  => Request,
                           Response => Response);
      end return;
   end Not_Found;

   ----------
   --  OK  --
   ----------

   function OK (Request       : in AWS.Status.Data;
                Response_Body : in JSON_Value := Create_Object)
                return AWS.Response.Data is
   begin

      Response_Body.Set_Field (Status_Text, OK_Reponse_Text);

      return Response : AWS.Response.Data do
         Response := AWS.Response.Build
           (Content_Type  => MIME_Types.JSON,
            Message_Body  => Response_Body.Write,
            Status_Code   => HTTP_Codes.OK,
            Cache_Control => AWS.Messages.No_Cache,
            Encoding      => AWS.Status.Preferred_Coding (Request));
         Add_CORS_Headers (Request  => Request,
                           Response => Response);
      end return;
   end OK;

   --------------------
   --  Server_Error  --
   --------------------

   function Server_Error (Response_Body : in JSON_Value := Create_Object)
                          return AWS.Response.Data is
      use AWS.Messages;
      use AWS.Response;

      Content : constant JSON_Value := Response_Body;
   begin
      Content.Set_Field (Status_Text, Server_Error_Reponse_Text);

      return Response : AWS.Response.Data do
         Response := AWS.Response.Build
           (Content_Type  => MIME_Types.JSON,
            Message_Body  => Response_Body.Write,
            Status_Code   => HTTP_Codes.Server_Error,
            Cache_Control => AWS.Messages.No_Cache,
            Encoding      => AWS.Messages.Identity);

         Set.Add_Header (D     => Response,
                         Name  => Access_Control_Allow_Origin_Token,
                         Value => "*");
         --  We don't have the origin information.

         Set.Add_Header (D     => Response,
                         Name  => Access_Control_Allow_Credentials_Token,
                         Value => "true");

         Set.Add_Header (D     => Response,
                         Name  => Access_Control_Allow_Headers_Token,
                         Value => "POST, GET");

         Set.Add_Header (D     => Response,
                         Name  => Access_Control_Max_Age_Token,
                         Value => "86400");
      end return;
   end Server_Error;

   function Server_Error (Request       : in AWS.Status.Data;
                          Response_Body : in JSON_Value := Create_Object)
                          return AWS.Response.Data is
      Content : constant JSON_Value := Response_Body;
   begin
      Content.Set_Field (Status_Text, Server_Error_Reponse_Text);

      return Response : AWS.Response.Data do
         Response := AWS.Response.Build
           (Content_Type  => MIME_Types.JSON,
            Message_Body  => Response_Body.Write,
            Status_Code   => HTTP_Codes.Server_Error,
            Cache_Control => AWS.Messages.No_Cache,
            Encoding      => AWS.Status.Preferred_Coding (Request));
         Add_CORS_Headers (Request  => Request,
                           Response => Response);
      end return;
   end Server_Error;

end Response.Templates;
