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

with Black.HTTP,
     Black.Request,
     Black.Response;

with Model.User;

private
with Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Strings.Hash;

private
package Handlers.Authenticated_Dispatcher is

   Package_Name : constant String := "Handlers.Authenticated_Dispatcher";

   function Run (Request : in Black.Request.Instance) return Black.Response.Instance;

   procedure Set_Default (Method : in Black.HTTP.Methods;
                          Action : in Black.Response.Callback);

   procedure Set_Default (Action : in Black.Response.Callback);

   type ACL (Public : Boolean) is
      record
         case Public is
            when True  => null;
            when False => As : Model.User.Permission_List;
         end case;
      end record;

   procedure Register (Method  : in Black.HTTP.Methods;
                       URI     : in String;
                       Allowed : in ACL;
                       Action  : in Black.Response.Callback);
private
   function Key (Method   : in     Black.HTTP.Methods;
                 Resource : in     String) return String;

   function Not_Authorized (Request : in     Black.Request.Instance)
                            return Black.Response.Instance;

   type Handler (Public : Boolean) is
      record
         Allowed : ACL (Public);
         Action  : Black.Response.Callback;
      end record;

   package Handler_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
            (Key_Type        => String,
             Element_Type    => Handler,
             Hash            => Ada.Strings.Hash,
             Equivalent_Keys => "=");

   Default_Action : array (Black.HTTP.Methods) of Black.Response.Callback
                      := (others => Not_Authorized'Access);
   Handler_List   : Handler_Maps.Map;
end Handlers.Authenticated_Dispatcher;
