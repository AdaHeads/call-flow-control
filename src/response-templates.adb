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

with View;
with HTTP_Codes;

package body Response.Templates is

   package HTTP renames HTTP_Codes;

   ----------------------
   --  Bad_Parameters  --
   ----------------------

   function Bad_Parameters (Request : in AWS.Status.Data;
                            Response_Body : in JSON_Value := Create_Object)
                            return AWS.Response.Data is
   begin

      Response_Body.Set_Field (Status_Text, Bad_Parameters_Reponse_Text);

      return AWS.Response.Build
        (Content_Type  => JSON_MIME_Type,
         Message_Body  => Response_Body.Write,
         Status_Code   => HTTP_Codes.Bad_Request,
         Cache_Control => AWS.Messages.No_Cache,
         Encoding      => AWS.Status.Preferred_Coding (Request));
   end Bad_Parameters;

   ----------------------
   --  Not_Authorized  --
   ----------------------

   function Not_Authorized (Request       : in AWS.Status.Data)
                            return AWS.Response.Data is
      Response_Body : JSON_Value := Create_Object;
   begin

      Response_Body.Set_Field (Status_Text, Not_Authorized_Reponse_Text);

      return AWS.Response.Build
        (Content_Type  => JSON_MIME_Type,
         Message_Body  => Response_Body.Write,
         Status_Code   => HTTP_Codes.Unauthorized,
         Cache_Control => AWS.Messages.No_Cache,
         Encoding      => AWS.Status.Preferred_Coding (Request));
   end Not_Authorized;

   -----------------
   --  Not_Found  --
   -----------------

   function Not_Found (Request       : in AWS.Status.Data;
                       Response_Body : in JSON_Value := Create_Object)
                       return AWS.Response.Data is
   begin
      Response_Body.Set_Field (Status_Text, Not_Found_Reponse_Text);

      return AWS.Response.Build
        (Content_Type  => JSON_MIME_Type,
         Message_Body  => Response_Body.Write,
         Status_Code   => HTTP_Codes.Not_Found,
         Cache_Control => AWS.Messages.No_Cache,
         Encoding      => AWS.Status.Preferred_Coding (Request));
   end Not_Found;

   ----------
   --  OK  --
   ----------

   function OK (Request       : in AWS.Status.Data;
                Response_Body : in JSON_Value := Create_Object)
                return AWS.Response.Data is
   begin

      Response_Body.Set_Field (Status_Text, OK_Reponse_Text);

      return AWS.Response.Build
        (Content_Type  => JSON_MIME_Type,
         Message_Body  => Response_Body.Write,
         Status_Code   => HTTP_Codes.OK,
         Cache_Control => AWS.Messages.No_Cache,
         Encoding      => AWS.Status.Preferred_Coding (Request));
   end OK;

   --------------------
   --  Server_Error  --
   --------------------

   function Server_Error (Response_Body : in JSON_Value := Create_Object)
                          return AWS.Response.Data is
      Content : constant JSON_Value := Response_Body;
   begin
      Content.Set_Field (Status_Text, Server_Error_Reponse_Text);

      return AWS.Response.Build
        (Content_Type  => JSON_MIME_Type,
         Message_Body  => Response_Body.Write,
         Status_Code   => HTTP_Codes.Server_Error,
         Cache_Control => AWS.Messages.No_Cache,
         Encoding      => AWS.Messages.Identity);
   end Server_Error;

   function Server_Error (Request       : in AWS.Status.Data;
                          Response_Body : in JSON_Value := Create_Object)
                          return AWS.Response.Data is
   begin

      return AWS.Response.Build
        (Content_Type  => JSON_MIME_Type,
         Message_Body  => Response_Body.Write,
         Status_Code   => HTTP_Codes.Server_Error,
         Cache_Control => AWS.Messages.No_Cache,
         Encoding      => AWS.Status.Preferred_Coding (Request));
   end Server_Error;

end Response.Templates;
