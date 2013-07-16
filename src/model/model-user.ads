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

with Ada.Containers.Indefinite_Vectors;

with AWS.URL;

package Model.User is

   type Name is new String
     with Dynamic_Predicate => (Name'Length > 0);

   type OpenID is new AWS.URL.Object;
   package OpenID_Lists is new Ada.Containers.Indefinite_Vectors
                                 (Element_Type => OpenID,
                                  Index_Type   => Positive);
   subtype OpenID_List is OpenID_Lists.Vector;

   type Permission is (Receptionist, Service_Agent, Administrator);
   type Permission_List is array (Permission) of Boolean;

   function OpenIDs (User : in     Name) return OpenID_List;

   function Permissions (User : in     Name) return Permission_List;

end Model.User;
