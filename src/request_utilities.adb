-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2013-, AdaHeads K/S                     --
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

with AWS.Client;
with AWS.Response;
with GNATCOLL.JSON;
with Alice_Configuration;

with System_Messages;

package body Request_Utilities is
   use AWS.Client;
   use Alice_Configuration;

   ----------------
   --  Token_Of  --
   ----------------

   function Token_Of (Request : in AWS.Status.Data)
                      return Model.Token.Instance is
      use AWS.Status;
      Token_String  : String renames Parameters (Request).Get ("token");
   begin
      return Model.Token.Create (Value => Token_String);
   end Token_Of;

   ---------------
   --  User_Of  --
   ---------------

   function User_Of (Request : in AWS.Status.Data)
                     return Model.User.Instance is
      use Model;

      Context       : constant String := Package_Name & ".User_Of";

      Request_Token : Token.Instance renames Token_Of (Request => Request);
      In_JSON       : GNATCOLL.JSON.JSON_Value;

      URL           : constant String :=
        Config.Get (Auth_Server) & "/token/" & Request_Token.To_String;
      Response   : AWS.Response.Data;
   begin

      Response := AWS.Client.Get (URL => URL);

      System_Messages.Debug
        (Message => URL & AWS.Response.Status_Code (Response)'Img,
         Context => Context);

      In_JSON := GNATCOLL.JSON.Read
        (Strm => AWS.Response.Message_Body (Response));

      return Model.User.Create (In_JSON);
   exception
      when others =>
         System_Messages.Error (Message => "User lookup failed!",
                                Context => Context);
         return Model.User.No_User;
   end User_Of;

end Request_Utilities;
