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

with GNATCOLL.JSON;

with Common,
     HTTP_Codes,
     View;

package body Response.Error_Messages is
   procedure Bad_Parameters (HTTP_Response : in out Object) is
      use GNATCOLL.JSON;
      JSON : JSON_Value;
   begin
      JSON := Create_Object;
      JSON.Set_Field (Field_Name => View.Status,
                      Field      => "bad parameters");

      HTTP_Status_Code (HTTP_Response,
                        HTTP_Codes.Bad_Request);
      Content (HTTP_Response,
               Common.To_JSON_String (JSON));
   end Bad_Parameters;

   procedure Not_Authorized (HTTP_Response : in out Object) is
      use GNATCOLL.JSON;
      JSON : JSON_Value;
   begin
      JSON := Create_Object;
      JSON.Set_Field (Field_Name => View.Status,
                      Field      => "not authorized");

      HTTP_Status_Code (HTTP_Response,
                        HTTP_Codes.Unauthorized);
      Content (HTTP_Response,
               Common.To_JSON_String (JSON));
   end Not_Authorized;

   procedure Too_Many_Parameters (HTTP_Response : in out Object) is
      use GNATCOLL.JSON;
      JSON : JSON_Value;
   begin
      JSON := Create_Object;
      JSON.Set_Field (Field_Name => View.Status,
                      Field      => "too many parameters");

      HTTP_Status_Code (HTTP_Response,
                        HTTP_Codes.Bad_Request);
      Content (HTTP_Response,
               Common.To_JSON_String (JSON));
   end Too_Many_Parameters;
end Response.Error_Messages;
