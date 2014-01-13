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

with JSON;

with Model.User;

package View.User is
   subtype User_Name_Labels is String
     with Dynamic_Predicate => (User_Name_Labels = View.Name or
                                User_Name_Labels = View.User_S);

   function To_JSON (Item  : in     Model.User.Name;
                     Label : in     User_Name_Labels)
                    return JSON.JSON_Value;

   function To_JSON (Item  : in     Model.User.Instance)
                    return JSON.JSON_Value;

   subtype OpenID_URL_Labels is String
     with Dynamic_Predicate => (OpenID_URL_Labels = View.OpenID or
                                OpenID_URL_Labels = View.URL);

   function To_JSON (Item  : in     Model.User.OpenID;
                     Label : in     OpenID_URL_Labels)
                    return JSON.JSON_Value;

   function To_JSON (Item  : in     Model.User.OpenID_List)
                    return JSON.JSON_Array;

end View.User;
