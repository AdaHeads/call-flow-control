-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                             Response.Cached                               --
--                                                                           --
--                                  BODY                                     --
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

--  with Ada.Strings.Fixed;
--  with AWS.Response.Set;
with AWS.URL;
with System_Message.Error;

package body Response.Cached is

   -------------------------------
   --  Generic_Cached_Response  --
   -------------------------------

   ----------------
   --  Generate  --
   ----------------

   function Generate
     (Request : in AWS.Status.Data)
         return AWS.Response.Data
   is
      use AWS.Status;
      use HTTP_Codes;
      use System_Message;

      Cache_Key       : Natural;
      Response_Object : Object := Factory (Request);
      Valid_Cache     : Boolean;
   begin
      Cache_Key := Get_Cache_Key (Response_Object);

      Read_From_Cache (Key      => Cache_Key,
                       Is_Valid => Valid_Cache,
                       Value    => Response_Object.Content);

      if not Valid_Cache then
         To_JSON (Response_Object => Response_Object);

         if Response_Object.Is_Cacheable then
            Write_To_Cache (Key   => Cache_Key,
                            Value => Response_Object.Content);
         end if;
      end if;

      return Response_Object.Build;
   exception
      when Event : others =>
         Notify (Error.Response_Generate_Error,
                 Event,
                 AWS.URL.URL (URI (Response_Object.Get_Request)),
                 Response_Object);
         return Response_Object.Build;
   end Generate;

end Response.Cached;
