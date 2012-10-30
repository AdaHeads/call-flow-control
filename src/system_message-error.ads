-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                           System_Message.Error                            --
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

with HTTP_Codes;

package System_Message.Error is

   Bad_Ce_Id_Key : Error_Log_And_Response_Object := Create
     (Description => "ce_id must be a valid natural integer",
      Status      => "bad request",
      Status_Code => HTTP_Codes.Bad_Request);

   Bad_Org_Id_Key : Error_Log_And_Response_Object := Create
     (Description => "org_id must be a valid natural integer",
      Status      => "bad request",
      Status_Code => HTTP_Codes.Bad_Request);

   Generic_Constraint_Error : Error_Log_And_Response_Object := Create
     (Description => "a constraint was violated",
      Status      => "bad request",
      Status_Code => HTTP_Codes.Bad_Request);

   Unknown_Error : Error_Log_And_Response_Object := Create
     (Description => "Exception raised while trying to generate content",
      Status      => "Server error",
      Status_Code => HTTP_Codes.Server_Error);

end System_Message.Error;
