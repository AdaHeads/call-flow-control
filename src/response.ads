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

with Ada.Strings.Unbounded;

with GNATCOLL.JSON;

package Response is
   use Ada.Strings.Unbounded;
   use GNATCOLL.JSON;

   Status_Text                  : constant String := "status";
   Bad_Parameters_Response_Text : constant String := "bad parameters";
   Not_Authorized_Response_Text : constant String := "not authorized";
   Forbidden_Response_Text      : constant String := "forbidden";
   Not_Found_Response_Text      : constant String := "not found";
   OK_Response_Text             : constant String := "ok";
   Server_Error_Response_Text   : constant String := "unhandled exception";

   Description_Text             : constant String := "description";

   type Statuses is (Unknown_Resource, Bad_Request, Permission_Denied,
                     Not_Found, Internal_Error, Success);

   type Instance is tagged
      record
         Keep_Open    : Boolean  := False;
         Status       : Statuses := Internal_Error;
         Description  : Unbounded_String;
         Current_Body : JSON_Value;
      end record;

   function Create (Status      : in Statuses;
                    Description : in String     := "";
                    With_Body   : in JSON_Value := Create_Object)
                    return Instance;

   function To_JSON_String (Object : in Instance) return String;

   function To_JSON (Object : in Instance) return JSON_Value;

   function Image (Object : in Instance) return String;

end Response;
