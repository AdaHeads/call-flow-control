-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                Response                                   --
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

with AWS.Response;
with AWS.Status;
with Common;
with Storage;

package Response is

   procedure Check_Ce_Id_Parameter
     (Request : in AWS.Status.Data)
   with inline;
   --  Check if the request parameter ce_id is numeric. Raise
   --  GET_Parameter_Error if not.

   function Get_Ce_Id_Key
     (Request : in AWS.Status.Data)
      return String
   with inline;
   --  Return the value of the ce_id request parameter.

   generic

      with procedure Check_Request_Parameters
        (Request : in AWS.Status.Data);
      --  Check the validity of all required request parameters.
      --  Must raise the Errors.GET_Parameter_Error exception if one or more
      --  the request parameters aren't valid.

      with function Get_Key
        (Request : in AWS.Status.Data)
      return String;
      --  Return the key used to identify an object in a cache.

      with procedure Read_From_Cache
        (Key      : in     String;
         Is_Valid :    out Boolean;
         Value    :    out Common.JSON_String);
      --  Find Key in a cache.

      with package Store is new Storage.Generic_Read (<>);
      --  This package enables reading data from persistent storage.

   package Generic_Read is

      function Get
        (Request : in AWS.Status.Data)
      return AWS.Response.Data;

   end Generic_Read;

end Response;
