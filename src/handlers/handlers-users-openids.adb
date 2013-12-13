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
     Model.User,
     Response.Error_Messages,
     Response.Not_Cached,
     View.User;

package body Handlers.Users.OpenIDs is

   ----------------------------------------------------------------------------

   procedure Generate_Document (Instance : in out Response.Object);
   --  Add a generated JSON_String to Response_Object.

   function JSON_Response is
      new Response.Not_Cached.Generate_Response
            (Generate_Document => Generate_Document);
   --  Generate the AWS.Response.Data that ultimately is delivered to the user.

   ----------------------------------------------------------------------------

   function Callback return AWS.Response.Callback is
   begin
      return JSON_Response'Access;
   end Callback;

   procedure Generate_Document
     (Instance : in out Response.Object)
   is
      use GNATCOLL.JSON;
      use Common;

      Bad_Parameters : exception;

      function Parameters_Okay return Boolean;
      function User_Name return Model.User.Name;

      function Parameters_Okay return Boolean is
      begin
         return Instance.Parameter_Count = 1 and
                Instance.Parameter_Exist ("user");
      exception
         when others =>
            raise Bad_Parameters;
      end Parameters_Okay;

      function User_Name return Model.User.Name is
      begin
         return Model.User.Name (Instance.Parameter ("user"));
      exception
         when others =>
            raise Bad_Parameters;
      end User_Name;

      Data : JSON_Value;
   begin
      Data := Create_Object;

      if Parameters_Okay then
         Data.Set_Field (Field_Name => View.Status,
                         Field      => "okay");
         Data.Set_Field (Field_Name => View.User_S,
                         Field      => String (User_Name));
--           Data.Set_Field (Field_Name => View.OpenIDs,
--                           Field      => View.User.To_JSON
--                                           (Model.User.OpenIDs (User_Name)));

         Instance.Content (To_JSON_String (Data));
      else
         Response.Error_Messages.Bad_Parameters (Instance);
      end if;
   exception
      when Bad_Parameters =>
         Response.Error_Messages.Bad_Parameters (Instance);
   end Generate_Document;

end Handlers.Users.OpenIDs;
