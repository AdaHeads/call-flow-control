-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Request                                   --
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

with AWS.Status;
with AWS.Response;

package Request is

   function Company
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Get the company JSON based on the "o" GET parameter.

   function Person
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Get a person JSON based on the "o" and "cn" GET parameters.

   function Persons
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;
   --  Get the persons JSON based on the "o" GET parameter.

end Request;
