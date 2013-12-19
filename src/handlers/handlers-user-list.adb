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
     Model.User.List,
     Response.Error_Messages,
     Response.Not_Cached,
     View.Users;

package body Handlers.User.List is

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

      Data : JSON_Value;
   begin
      Data := Create_Object;

      Data.Set_Field (Field_Name => View.Status,
                      Field      => View.OK);
      Data.Set_Field (Field_Name => View.Users_S,
                      Field      => Model.User.List.Get_Singleton.To_JSON);

      Instance.Content (To_JSON_String (Data));
   end Generate_Document;

end Handlers.User.List;
