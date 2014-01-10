-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                     --
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

with JSON;

with Common,
     HTTP_Codes,
     Response,
     View,
     Model.Token.List,
     Model.Peer.List;

package body Handlers.Debug is
   use Common;

   function Channel_List (Request : in AWS.Status.Data)
                          return AWS.Response.Data is
      use JSON;
      use HTTP_Codes;

      Response_Object : Response.Object := Response.Factory (Request);
      Data            : JSON_Value;
   begin

      Response_Object.HTTP_Status_Code (OK);
      --  TODO:
      Data := Create_Object;
      Data.Set_Field (Field_Name  => View.Status,
                      Field       => "Not implemented");
      Response_Object.Content (To_JSON_String (Data));

      return Response_Object.Build;
   end Channel_List;

   function Dummy_Response
     (Request : in AWS.Status.Data)
      return AWS.Response.Data is
      Response_Object : Response.Object := Response.Factory (Request);
   begin
      Response_Object.HTTP_Status_Code (HTTP.OK);

      return Response_Object.Build;
   end Dummy_Response;

   function Dummy_Response_No_Content
     (Request : in AWS.Status.Data)
      return AWS.Response.Data is
      Response_Object : Response.Object := Response.Factory (Request);
   begin
      Response_Object.HTTP_Status_Code (HTTP.No_Content);

      return Response_Object.Build;
   end Dummy_Response_No_Content;

   function Dummy_Tokens (Request : in AWS.Status.Data)
                       return AWS.Response.Data is
      use HTTP_Codes;

      Response_Object : Response.Object := Response.Factory (Request);
   begin
      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content (Model.Token.List.Get_Singleton.To_JSON);

      return Response_Object.Build;
   end Dummy_Tokens;

   function Peer_List (Request : in AWS.Status.Data)
                       return AWS.Response.Data is
      use HTTP_Codes;

      Response_Object : Response.Object := Response.Factory (Request);
   begin
      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content
        (To_JSON_String (Model.Peer.List.Get_Singleton.To_JSON));

      return Response_Object.Build;
   end Peer_List;

end Handlers.Debug;
