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
     Model.User,
     Response.Error_Messages,
     Response.Not_Cached,
     View.User;

package body Handlers.Users.OpenIDs is

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

   ----------------------------------------------------------------------------

   procedure Generate_Document (Instance : in out Response.Object);
   --  Add a generated JSON_String to Response_Object.

   function JSON_Response is
      new Response.Not_Cached.Generate_Response
            (Public            => Public_User_Information,
             Allowed           => (Model.User.Receptionist  => False,
                                   Model.User.Service_Agent => False,
                                   Model.User.Administrator => True),
             Generate_Document => Generate_Document);
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

      Bad_Parameters : exception;

      function Parameters_Okay return Boolean;
      function User_Name return Model.User.Name;

      function Public_User_Information return Boolean;

      function Parameters_Okay return Boolean is
      begin
         return Instance.Parameter_Count = 1 and
                Instance.Parameter_Exist ("user");
      exception
         when others =>
            raise Bad_Parameters;
      end Parameters_Okay;

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

      if Public_User_Information then
         if Parameters_Okay then
            Data.Set_Field (Field_Name => View.Status,
                            Field      => "okay");
            Data.Set_Field (Field_Name => View.User_S,
                            Field      => String (User_Name));
            Data.Set_Field (Field_Name => View.OpenIDs,
                            Field      => View.User.To_JSON
                                            (Model.User.OpenIDs (User_Name)));

            Instance.Content (To_JSON_String (Data));
         else
            Response.Error_Messages.Bad_Parameters (Instance);
         end if;
      else
         Response.Error_Messages.Not_Authorized (Instance);
      end if;
   exception
      when Bad_Parameters =>
         Response.Error_Messages.Bad_Parameters (Instance);
   end Generate_Document;

end Handlers.Users.OpenIDs;
