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

with System_Message.Critical;

package body Response.Cached is

   -------------------------
   --  Generate_Response  --
   -------------------------

   function Generate_Response
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use System_Message;

      Key         : Cache_Key_Type;
      Response    : Object := Factory (Request);
      Valid_Cache : Boolean;
   begin
      Set_Request_Parameters (Response);
      Response.Validate_Request_Parameters;

      if Response.Valid_Request_Parameters then
         Key := Cache_Key (Response);

         Cache.Read (Key      => Key,
                     Is_Valid => Valid_Cache,
                     Value    => Response.Content);

         if not Valid_Cache then
            Generate_Document (Response);

            if Response.Is_Cacheable then
               Cache.Write (Key   => Key,
                            Value => Response.Content);
            end if;
         end if;
      end if;

      return Response.Build;
   exception
      when Event : others =>
         --  For now we assume that "other" exceptions caught here are bad
         --  enough to warrant a critical level log entry and response.
         Critical.Response_Exception
           (Event           => Event,
            Message         => Response.To_Debug_String,
            Response_Object => Response);

         return Response.Build;
   end Generate_Response;

end Response.Cached;
