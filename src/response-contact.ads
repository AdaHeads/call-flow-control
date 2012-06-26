-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                             Response.Contact                              --
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

with Common;

package Response.Contact is

   function Bad_Parameters
     (Request : AWS.Status.Data)
      return Boolean;
   --  TODO.

   function Get_Key
     (Request : in AWS.Status.Data)
      return String;
   --  TODO.

   procedure Read_Cache
     (Key      : in     String;
      Is_Valid :    out Boolean;
      Value    :    out Response_Generic.JSON_Bounded_String);
   --  TODO.

   procedure Storage_Read
     (Key    : in String;
      Status : out AWS.Messages.Status_Code;
      Value  : out JSON_Bounded_String);
   --  TODO.

   function To_String
     (Value : in JSON_Bounded_String)
      return String;
   --  TODO.

   package Contact is new Response_Generic
     (JSON_Bounded_String => Common.JSON_Small.Bounded_String,
      Bad_Parameters      => Bad_Parameters,
      Get_Key             => Get_Key,
      Read_Cache          => Read_Cache,
      Storage_Read        => Storage_Read,
      To_String           => To_String);

end Response.Contact;
