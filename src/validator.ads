-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                Validator                                  --
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

with Model;

package Validator is

   type Object is tagged private;

   type Value_Type  is (Contact_Identifier, Organization_Identifier);

   procedure Register
     (Instance    : in out Object;
      Parameter   : in String;
      Validate_As : in Value_Type);
   --  TODO: Write comment.

private

   type Values (Kind : Value_Type) is
      record
         case Kind is
         when Contact_Identifier =>
            CI_Value : Model.Contact_Identifier;
         when Organization_Identifier =>
            OI_Value : Model.Organization_Identifier;
         end case;
      end record;

   type Set is
      record

      end record;

   type Object is tagged
      record
         null;
      end record;

end Validator;
