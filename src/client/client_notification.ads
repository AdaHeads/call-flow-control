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

with GNATCOLL.JSON;

package Client_Notification is
   use GNATCOLL.JSON;

   type Instance (Persistent : Boolean) is abstract tagged private;

   function JSON_Root (O : in Instance'Class) return JSON_Value;
   --  Class-wide operation that returns the root of the notification
   --  in JSON format.

   procedure JSON_Append (Node  : in JSON_Value;
                          Key   : in String;
                          Value : in JSON_Value);
   --  Appends additional JSON objects to the body.

   function To_JSON (O : in Instance) return JSON_Value is abstract;
   --  Mandatory individual JSON conversion function.

   function Header_Name (O : in Instance) return String is abstract;
   --  Mandatory header name.

private
   type Instance (Persistent : Boolean) is abstract tagged null record;

end Client_Notification;
