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

with Request_Utilities,
     Response,
     Response.Templates,
     System_Messages;

package body Handlers.Authenticated_Dispatcher is

   -----------
   --  Key  --
   -----------

   function Key (Method   : in Black.HTTP.Methods;
                 Resource : in String) return String is
   begin
      return Black.HTTP.Methods'Image (Method) & ":" & Resource;
   end Key;

   ----------------------
   --  Not_Authorized  --
   ----------------------

   function Not_Authorized (Request : in Black.Request.Instance)
                            return Black.Response.Class is
   begin
      return Response.Templates.Not_Authorized (Request => Request);
   end Not_Authorized;

   ----------------
   --  Register  --
   ----------------
   procedure Register (Method  : in     Black.HTTP.Methods;
                       URI     : in     String;
                       Allowed : in     ACL;
                       Action  : in     HTTP.Callback) is
   begin
      Handler_List.Insert (Key      => Key (Method   => Method,
                                            Resource => URI),
                           New_Item => (Public  => Allowed.Public,
                                        Allowed => Allowed,
                                        Action  => Action));
   end Register;

   -----------
   --  Run  --
   -----------

   function Run (Request : in Black.Request.Instance)
                 return Black.Response.Class is
      use Model;
      use Model.User;

      Context     : constant String := Package_Name & ".Run";

      Request_Key : constant String := Key (Method   => Request.Method,
                                            Resource => Request.Resource);
   begin

      if Handler_List.Contains (Request_Key) then
         declare
            Selected : constant Handler := Handler_List.Element (Request_Key);
            Allowed  : ACL renames Selected.Allowed;
            Detected_User : constant User.Instance :=
              Request_Utilities.User_Of (Request);
         begin

            if Allowed.Public then
               return Selected.Action (Request);
            elsif
              (Detected_User.Permissions and Allowed.As) = No_Permissions
              or
                Detected_User = No_User
            then
               return Not_Authorized (Request);
            else
               return Selected.Action (Request);
            end if;
         end;
      else
         return Default_Action (Request.Method) (Request);
      end if;
   exception
      when Event : others =>
         --  For now we assume that "other" exceptions caught here are bad
         --  enough to warrant a critical level log entry and response.
         System_Messages.Critical_Exception
           (Message => "Request failed!",
            Event   => Event,
            Context => Context);

         return Response.Templates.Not_Authorized (Request => Request);
   end Run;

   -------------------
   --  Set_Default  --
   -------------------

   procedure Set_Default (Method : in Black.HTTP.Methods;
                          Action : in HTTP.Callback) is
   begin
      Default_Action (Method) := Action;
   end Set_Default;

   -------------------
   --  Set_Default  --
   -------------------

   procedure Set_Default (Action : in HTTP.Callback) is
   begin
      Default_Action := (others => Action);
   end Set_Default;
end Handlers.Authenticated_Dispatcher;
