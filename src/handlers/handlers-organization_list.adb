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

with AWS.Status;
with HTTP_Codes;
with Model.Organizations;
with System_Message.Error;

package body Handlers.Organization_List is

   -------------------------------
   --  Bad_List_View_Parameter  --
   -------------------------------

   procedure Bad_List_View_Parameter
     (Response_Object :    out Response.Object;
      Message         : in     String)
   is
      use System_Message;
   begin
      Error.Bad_Organization_List_View (Message         => Message,
                           Response_Object => Response_Object);
   end Bad_List_View_Parameter;

   ----------------
   --  Callback  --
   ----------------

   function Callback
     return AWS.Dispatchers.Callback.Handler
   is
   begin
      return AWS.Dispatchers.Callback.Create (JSON_Response'Access);
   end Callback;

   -------------------------
   --  Generate_Document  --
   -------------------------

   procedure Generate_Document
     (Response_Object : in out Response.Object)
   is
      use Common;
      use HTTP_Codes;
      use Model.Organizations;

      Organization_List : List;
   begin
      case Get_List_View (Response_Object) is
         when Mini =>
            Organization_List := Get (Mini);
         when Midi =>
            Organization_List := Get (Midi);
      end case;

      if Organization_List /= Null_List then
         Response_Object.Cacheable (True);
         Response_Object.HTTP_Status_Code (OK);
      else
         Response_Object.HTTP_Status_Code (Not_Found);
      end if;

      Response_Object.Content (Organization_List.To_JSON_String);
   end Generate_Document;

   ---------------------
   --  Get_List_Kind  --
   ---------------------

   function Get_List_View
     (Response_Object : in Response.Object)
      return View_Type
   is
      use AWS.Status;
   begin
      if Parameters (Response_Object.Status_Data).Count = 0 then
         return Mini;
      end if;

      return View_Type'Value
        (Parameters (Response_Object.Status_Data).Get ("view"));
   end Get_List_View;

end Handlers.Organization_List;