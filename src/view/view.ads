-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                  View                                     --
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

package View is
   pragma Pure;

   type Mode is (Basic, Full);

   Arrival_Time_S    : constant String := "arrival_time";
   Assigned_To_S     : constant String := "assigned_to";
   Attributes        : constant String := "attributes";
   Call_S            : constant String := "call";
   Call_ID_S         : constant String := "call_Id";
   Channel           : constant String := "channel";
   Contact_Id        : constant String := "contact_id";
   Contacts          : constant String := "contacts";
   Full_Name         : constant String := "full_name";
   Identifier        : constant String := "identifier";
   Inbound           : constant String := "inbound";
   Is_Human          : constant String := "is_human";
   Organization_Id   : constant String := "organization_id";
   Organization_List : constant String := "organization_list";
   Length            : constant String := "length";
   State_S           : constant String := "state";
   Status            : constant String := "status";

end View;
