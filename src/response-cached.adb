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

      function Found_Cache_Key
        return Boolean;

      Key             : Cache_Key_Type;
      Response_Object : Object := Factory (Request);
      Valid_Cache     : Boolean;

      function Found_Cache_Key
        return Boolean
      is
      begin
         Key := Cache_Key (Response_Object);
         return True;
      exception
         when others =>
            Bad_Request_Parameters (Response_Object,
                                    Response_Object.Request_URL);
            return False;
      end Found_Cache_Key;
   begin
      if Found_Cache_Key then
         Cache.Read (Key      => Key,
                     Is_Valid => Valid_Cache,
                     Value    => Response_Object.Content);

         if not Valid_Cache then
            Generate_Document (Response_Object);

            if Response_Object.Is_Cacheable then
               Cache.Write (Key   => Key,
                            Value => Response_Object.Content);
            end if;
         end if;
      end if;

      return Response_Object.Build;
   exception
      when Event : others =>
         --  For now we assume that "other" exceptions caught here are bad
         --  enough to warrant a critical level log entry and response.
         Critical.Response_Exception
           (Event           => Event,
            Message         => Response_Object.To_Debug_String,
            Response_Object => Response_Object);
         return Response_Object.Build;
   end Generate_Response;

end Response.Cached;
