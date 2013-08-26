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

with HTTP_Codes,
     Model.Organizations,
     Request_Parameters;

package body Handlers.Organization_List is

   -----------------
   --  Cache_Key  --
   -----------------

   function Cache_Key
     (Instance : in Response.Object)
      return Request_Parameters.List_View;
   function Cache_Key
     (Instance : in Response.Object)
      return Request_Parameters.List_View
   is
      use Request_Parameters;
   begin
      --  View isn't a required request parameter, so we have to check for it
      --  before trying to cast it to an Organization_List_View;
      if Instance.Parameter_Exist ("view") then
         return List_View'Value
           (Instance.Parameter ("view"));
      else
         return Mini;
      end if;
   end Cache_Key;

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
     (Instance : in out Response.Object)
   is
      use HTTP_Codes;
      use Model.Organizations;

      Organization_List : List;
   begin
      case Cache_Key (Instance) is
         when Request_Parameters.Mini =>
            Organization_List := Get (Request_Parameters.Mini);
         when Request_Parameters.Midi =>
            Organization_List := Get (Request_Parameters.Midi);
      end case;

      if Organization_List /= Null_List then
         Instance.Is_Cacheable (True);
         Instance.HTTP_Status_Code (OK);
      else
         Instance.HTTP_Status_Code (Not_Found);
      end if;

      Instance.Content (Organization_List.To_JSON_String);
   end Generate_Document;

   ------------------------------
   --  Set_Request_Parameters  --
   ------------------------------

   procedure Set_Request_Parameters
     (Instance : out Response.Object)
   is
      use Response;
   begin
      Instance.Register_Request_Parameter
        (Mode           => Request_Parameters.Optional,
         Parameter_Name => "view",
         Validate_As    => Request_Parameters.Organization_List_View);
   end Set_Request_Parameters;

end Handlers.Organization_List;
