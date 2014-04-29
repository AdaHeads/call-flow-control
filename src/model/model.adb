-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
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

with Ada.Strings,
     Ada.Strings.Fixed;

package body Model is
   function Image (Item : in Reception_Contact_Identifier) return String is
      use Ada.Strings, Ada.Strings.Fixed;
   begin
      return
        Trim (Contact_Identifier'Image   (Item.Contact_ID),   Both) &
        "@" &
        Trim (Reception_Identifier'Image (Item.Reception_ID), Both);
   end Image;

   function Value (Item : in String) return Reception_Contact_Identifier is
      use Ada.Strings.Fixed;
      At_Count : Natural := 0;
      Split_At : Positive;
   begin
      Scan :
      for Index in Item'Range loop
         case Item (Index) is
            when '@' =>
               At_Count := At_Count + 1;
               Split_At := Index;
            when '0' .. '9' =>
               null;
            when others =>
               raise Constraint_Error
                 with """" & Item & """ is not an organisation contact " &
                      "identifier.";
         end case;
      end loop Scan;

      if At_Count = 1 and then Split_At in Item'First + 1 .. Item'Last - 1 then
         declare
            Contact_ID   : String renames Item (Item'First   .. Split_At - 1);
            Reception_ID : String renames Item (Split_At + 1 .. Item'Last);
         begin
            if Contact_ID = "" or Reception_ID = "" then
               raise Constraint_Error
                 with """" & Item & """ is not an organisation contact " &
                      "identifier.";
            else
               return
                 (Contact_ID   => Contact_Identifier'Value   (Contact_ID),
                  Reception_ID => Reception_Identifier'Value (Reception_ID));
            end if;
         end;
      else
         raise Constraint_Error
           with """" & Item & """ is not an organisation contact " &
           "identifier.";
      end if;
   end Value;
end Model;
