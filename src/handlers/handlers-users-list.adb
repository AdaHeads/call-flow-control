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

with Alice_Configuration,
     Common,
     HTTP_Codes,
     Model.Users,
     Response.Not_Cached,
     View.Users;

package body Handlers.Users.List is

   procedure Generate_Document (Instance : in out Response.Object);
   --  Add a generated JSON_String to Response_Object.

   function JSON_Response is
      new Response.Not_Cached.Generate_Response
            (Generate_Document => Generate_Document);
   --  Generate the AWS.Response.Data that ultimately is delivered to the user.

   ----------------------------------------------------------------------------

   function Callback return AWS.Dispatchers.Callback.Handler is
   begin
      return AWS.Dispatchers.Callback.Create (JSON_Response'Access);
   end Callback;

   procedure Generate_Document
     (Instance : in out Response.Object)
   is
      use GNATCOLL.JSON;
      use Common;

      function Public_User_Information return Boolean;
      function Public_User_Information return Boolean is
         use Alice_Configuration;
      begin
         return Config.Get (Public_User_Information);
      exception
         when others =>
            raise Constraint_Error
              with "The 'Public_User_Information' configuration field is a " &
                   "Boolean.";
      end Public_User_Information;

      Data : JSON_Value;
   begin
      Data := Create_Object;

      if Public_User_Information then
         if Instance.Parameter_Count = 0 then
            Data.Set_Field (Field_Name => View.Status,
                            Field      => "okay");
            Data.Set_Field (Field_Name => View.Users_S,
                            Field      => View.Users.To_JSON
                                            (Model.Users.List));

            Instance.Content (To_JSON_String (Data));
         else
            Data.Set_Field (Field_Name => View.Status,
                            Field      => "too many parameters");

            Instance.Content (To_JSON_String (Data));
            Instance.HTTP_Status_Code (HTTP_Codes.Bad_Request);
         end if;
      else
         Data.Set_Field (Field_Name => View.Status,
                         Field      => "not authorized");

         Instance.Content (To_JSON_String (Data));
         Instance.HTTP_Status_Code (HTTP_Codes.Unauthorized);
      end if;
   end Generate_Document;

end Handlers.Users.List;
