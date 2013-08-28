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

with Common;
with HTTP_Codes,
     System_Message.Info;

package body CORS_Preflight is

   ----------------
   --  Callback  --
   ----------------

   function Callback
     return AWS.Response.Callback
   is
   begin
      return JSON_Response'Access;
   end Callback;

   -------------------------
   --  Generate_Document  --
   -------------------------

   procedure Generate_Document
     (Instance : in out Response.Object)
   is
      use Common;
      use HTTP_Codes;
   begin
      System_Message.Info.Jacob_Wants_To_See_This
        (Message => "CORS Preflight URI: " & Instance.Request_URL);

      Instance.HTTP_Status_Code (OK);
      Instance.Content (Null_JSON_String);
   end Generate_Document;

end CORS_Preflight;
