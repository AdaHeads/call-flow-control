-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                             My_Configuration                              --
--                                                                           --
--                                  SPEC                                     --
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                      --
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

with Ada.Strings.Unbounded;
with Yolk.Config_File_Parser;
with Yolk.Utilities;

package My_Configuration is

   use Yolk.Utilities;

   type Keys is (Handler_Get_Customer,
                 Handler_Get_Person);

   type Defaults_Array is array (Keys) of
     Ada.Strings.Unbounded.Unbounded_String;

   Default_Values : constant Defaults_Array :=
                      (Handler_Get_Customer
                       => TUS ("/get_customer"),
                       Handler_Get_Person
                       => TUS ("/get_person"));

   package Config is new Yolk.Config_File_Parser
     (Key_Type => Keys,
      Defaults_Array_Type => Defaults_Array,
      Defaults => Default_Values,
      Config_File => "configuration/alice_config.ini");

end My_Configuration;
