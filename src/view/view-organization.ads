-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                            View.Organization                              --
--                                                                           --
--                                  SPEC                                     --
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

with Common;
with GNATCOLL.JSON;
with Model.Organizations;

package View.Organization is

   use Common;
   use GNATCOLL.JSON;
   use Model.Organizations;

   function To_JSON
     (O    : in Organization_Object;
      View : in Mode)
      return JSON_Value;
   --  Convert O to a JSON object.
   --
   --  If View_Mode is View.Basic then the organization JSON document is NOT
   --  added to the final JSON object. This is handy in cases where only the
   --  id, identifier and full name of the organization is needed.
   --
   --  If View_Mode is View.Full, then the organization id, identifier, full
   --  name and all the organization contacts are added to the organization
   --  JSON document.

   function To_JSON
     (O    : in Organization_Object;
      View : in Mode)
      return JSON_String;
   --  Convert O to a JSON string.
   --
   --  If View_Mode is View.Basic then the organization JSON document is NOT
   --  added to the final JSON string. This is handy in cases where only the
   --  id, identifier and full name of the organization is needed.
   --
   --  If View_Mode is View.Full, then the organization id, identifier, full
   --  name and all the organization contacts are added to the organization
   --  JSON document.

   function To_JSON
     (O    : in Organization_List_Object;
      View : in Mode)
      return JSON_Value;
   --  Convert O into a JSON object.
   --
   --  If View_Mode is View.Basic then the organization JSON documents are NOT
   --  added to the final JSON object. This is handy in cases where only the
   --  id, identifier and full name of the organization is needed.
   --
   --  If View_Mode is View.Full, then the organization id, identifier and full
   --  name are added to the organization JSON document.
   --
   --  No matter the View_Mode, the organization contacts are NEVER added to
   --  the final JSON object.

   function To_JSON
     (O    : in Organization_List_Object;
      View : in Mode)
      return JSON_String;
   --  Convert O into a JSON string.
   --
   --  If View_Mode is View.Basic then the organization JSON documents are NOT
   --  added to the final JSON string. This is handy in cases where only the
   --  id, identifier and full name of the organization is needed.
   --
   --  If View_Mode is View.Full, then the organization id, identifier and full
   --  name are added to the organization JSON document.
   --
   --  No matter the View_Mode, the organization contacts are NEVER added to
   --  the final JSON string.

end View.Organization;
