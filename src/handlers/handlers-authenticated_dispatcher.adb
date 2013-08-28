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

with GNATCOLL.JSON;

with Common,
     Handlers.OpenID,
     HTTP_Codes,
     MIME_Types,
     View;

package body Handlers.Authenticated_Dispatcher is
   function Key (Method : in     AWS.Status.Request_Method;
                 URI    : in     String) return String is
      use AWS.Status;
   begin
      return Request_Method'Image (Method) & ":" & URI;
   end Key;

   function Not_Authorized (Request : in     AWS.Status.Data)
                           return AWS.Response.Data is
      pragma Unreferenced (Request);

      JSON : GNATCOLL.JSON.JSON_Value;
   begin
      JSON := GNATCOLL.JSON.Create_Object;
      JSON.Set_Field (Field_Name => View.Status,
                      Field      => "not authorized");

      return
        AWS.Response.Build (Content_Type => MIME_Types.JSON,
                            Message_Body => Common.To_String
                                              (Common.To_JSON_String (JSON)),
                            Status_Code  => HTTP_Codes.Unauthorized);
   end Not_Authorized;

   procedure Register (Method     : in     AWS.Status.Request_Method;
                       URI        : in     String;
                       Allowed    : in     Authentication;
                       Action     : in     AWS.Response.Callback) is
   begin
      Handler_List.Insert (Key      => Key (Method => Method,
                                            URI    => URI),
                           New_Item => (Public  => Allowed.Public,
                                        Allowed => Allowed,
                                        Action  => Action));
   end Register;

   function Run (Request : in     AWS.Status.Data) return AWS.Response.Data is
      use AWS.Status;
      use Model.User;

      Request_Key : constant String := Key (Method => Method (Request),
                                            URI    => URI (Request));
      Groups      : constant Permission_List := OpenID.Permissions (Request);
   begin
      if Handler_List.Contains (Request_Key) then
         declare
            Selected : constant Handler := Handler_List.Element (Request_Key);
            Allowed  : Authentication renames Selected.Allowed;
         begin
            if Allowed.Public then
               return Selected.Action (Request);
            elsif (Groups and Allowed.As) = No then
               return Not_Authorized (Request);
            else
               return Selected.Action (Request);
            end if;
         end;
      else
         return Default_Action (Method (Request)) (Request);
      end if;
   end Run;

   procedure Set_Default (Method : in     AWS.Status.Request_Method;
                          Action : in     AWS.Response.Callback) is
   begin
      Default_Action (Method) := Action;
   end Set_Default;

   procedure Set_Default (Action : in     AWS.Response.Callback) is
   begin
      Default_Action := (others => Action);
   end Set_Default;
end Handlers.Authenticated_Dispatcher;
