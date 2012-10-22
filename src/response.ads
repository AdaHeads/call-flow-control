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

with AWS.Messages;
with AWS.Response;
with AWS.Status;
with Common;
with HTTP_Codes;

package Response is

   type Object is tagged limited private;

   function Build
     (O : in Object)
      return AWS.Response.Data;
   --  Build the response and compress it if the client supports it. Also
   --  wraps JSON string in foo(JSON string) if the
   --      ?jsoncallback=foo
   --  GET parameter is present.

   function Factory
     (Request : in AWS.Status.Data)
      return Object;
   --  Initialize a Response.Object object.

   function Get_Request
     (O : in Object)
      return AWS.Status.Data;
   --  Return the AWS.Status.Data object that O was initialized with.

   procedure Set_Cacheable
     (O     :    out Object;
      Value : in     Boolean);
   --  Set whether the contents of O is cacheable.

   procedure Set_Content
     (O     :    out Object;
      Value : in     Common.JSON_String);
   --  Add content to O.

   procedure Set_HTTP_Status_Code
     (O     :    out Object;
      Value : in     AWS.Messages.Status_Code);
   --  Set the HTTP code that is returned to the client when O.Build is called.

private

   type Object is tagged limited
      record
         Content          : Common.JSON_String := Common.Null_JSON_String;
         HTTP_Status_Code : AWS.Messages.Status_Code := HTTP_Codes.OK;
         Is_Cacheable     : Boolean := False;
         Request          : AWS.Status.Data;
      end record;

end Response;
