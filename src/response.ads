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

generic

   type JSON_Bounded_String is private;

   with function Bad_Parameters
     (Request : in AWS.Status.Data)
         return Boolean;
   --  TODO.

   with function Get_Key
     (Request : in AWS.Status.Data)
         return String;
   --  TODO.

   with procedure Read_Cache
     (Key      : in     String;
      Is_Valid :    out Boolean;
      Value    :    out JSON_Bounded_String);
   --  TODO.

   with procedure Storage_Read
     (Key    : in String;
      Status : out AWS.Messages.Status_Code;
      Value  : out JSON_Bounded_String);
   --  TODO.

   with function To_String
     (Value : in JSON_Bounded_String)
         return String;
   --  TODO.

package Response is

   function Get
     (Request : in AWS.Status.Data)
         return AWS.Response.Data;
   --  TODO.

end Response;
