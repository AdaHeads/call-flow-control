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

with AWS.URL;
with System_Message.Error;

package body Response.Cached is

   -------------------------
   --  Generate_Response  --
   -------------------------

   function Generate_Response
     (Request : in AWS.Status.Data)
         return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.URL;
      use HTTP_Codes;
      use System_Message;

      function Found_Cache_Key
        return Boolean;

      Cache_Key       : Cache_Key_Type;
      Response_Object : Object := Factory (Request);
      Valid_Cache     : Boolean;

      function Found_Cache_Key
        return Boolean
      is
      begin
         Cache_Key := Get_Cache_Key (Response_Object);
         return True;
      exception
         when others =>
            Bad_Request_Parameters (Response_Object,
                                    URL (URI (Response_Object.Get_Request)));
            return False;
      end Found_Cache_Key;
   begin
      if Found_Cache_Key then
         Read_From_Cache (Key      => Cache_Key,
                          Is_Valid => Valid_Cache,
                          Value    => Response_Object.Content);

         if not Valid_Cache then
            Generate_Document (Response_Object);

            if Response_Object.Is_Cacheable then
               Write_To_Cache (Key   => Cache_Key,
                               Value => Response_Object.Content);
            end if;
         end if;
      end if;

      return Response_Object.Build;
   exception
      when Event : others =>
         Error.Unknown_Error.Notify (Event,
                                     URL (URI (Response_Object.Get_Request)),
                                     Response_Object);
         return Response_Object.Build;
   end Generate_Response;

end Response.Cached;
