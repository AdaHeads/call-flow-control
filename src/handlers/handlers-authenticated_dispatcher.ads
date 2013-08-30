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

with AWS.Response,
     AWS.Status;

with Model.User;

private
with Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Strings.Hash;

private
package Handlers.Authenticated_Dispatcher is
   function Run (Request : in     AWS.Status.Data) return AWS.Response.Data;

   procedure Set_Default (Method : in     AWS.Status.Request_Method;
                          Action : in     AWS.Response.Callback);

   procedure Set_Default (Action : in     AWS.Response.Callback);

   type Authentication (Public : Boolean) is
      record
         case Public is
            when True  => null;
            when False => As : Model.User.Permission_List;
         end case;
      end record;

   procedure Register (Method     : in     AWS.Status.Request_Method;
                       URI        : in     String;
                       Allowed    : in     Authentication;
                       Action     : in     AWS.Response.Callback);
private
   function Key (Method : in     AWS.Status.Request_Method;
                 URI    : in     String) return String;

   function Not_Authorized (Request : in     AWS.Status.Data)
                           return AWS.Response.Data;

   type Handler (Public : Boolean) is
      record
         Allowed : Authentication (Public);
         Action  : AWS.Response.Callback;
      end record;

   package Handler_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
            (Key_Type        => String,
             Element_Type    => Handler,
             Hash            => Ada.Strings.Hash,
             Equivalent_Keys => "=");

   Default_Action : array (AWS.Status.Request_Method) of AWS.Response.Callback
                      := (others => Not_Authorized'Access);
   Handler_List   : Handler_Maps.Map;
end Handlers.Authenticated_Dispatcher;
