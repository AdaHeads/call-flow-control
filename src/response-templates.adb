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

   function Bad_Parameters return Response.Object is
      use GNATCOLL.JSON;

      Content : constant JSON_Value := Create_Object;
   begin
      return Response_Object : Response.Object := Response.Factory do
         Content.Set_Field (Field_Name => View.Status,
                         Field      => Bad_Parameters_Reponse_Text);
         HTTP_Status_Code (Response_Object, HTTP.Bad_Request);
         Response.Content (Response_Object, Content);
      end return;
   end Bad_Parameters;

   ----------------------
   --  Not_Authorized  --
   ----------------------

   function Not_Authorized return Response.Object is
      use GNATCOLL.JSON;

      Content : constant JSON_Value := Create_Object;
   begin
      return Response_Object : Response.Object := Response.Factory do
         Content.Set_Field (Field_Name => View.Status,
                         Field      => Not_Authorized_Reponse_Text);
         HTTP_Status_Code (Response_Object, HTTP.Unauthorized);
         Response.Content (Response_Object, Content);
      end return;
   end Not_Authorized;

   -----------------
   --  Not_Found  --
   -----------------

   function Not_Found return Response.Object is
      use GNATCOLL.JSON;

      Content : constant JSON_Value := Create_Object;
   begin
      return Response_Object : Response.Object := Response.Factory do
         Content.Set_Field (Field_Name => View.Status,
                         Field      => Not_Found_Reponse_Text);
         HTTP_Status_Code (Response_Object, HTTP.Not_Found);
         Response.Content (Response_Object, Content);
      end return;
   end Not_Found;

   --------------------
   --  Server_Error  --
   --------------------

   function Server_Error return Response.Object is
      use GNATCOLL.JSON;

      Content : constant JSON_Value := Create_Object;
   begin
      return Response_Object : Response.Object := Response.Factory do
         Content.Set_Field (Field_Name => View.Status,
                            Field      => Not_Found_Reponse_Text);

         Content.Set_Field
           (Field_Name => View.Description,
            Field      =>
              "This is a disaster and should be fixed by someone soon");

         HTTP_Status_Code (Response_Object, HTTP.Server_Error);
         Response.Content (Response_Object, Content);
      end return;
   end Server_Error;

end Response.Templates;
