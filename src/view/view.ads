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

package View is
   pragma Pure;

   type Mode is (Basic, Full);

   Arrival_Time_S    : constant String := "arrival_time";
   Assigned_To_S     : constant String := "assigned_to";
   Attributes        : constant String := "attributes";
   Call_S            : constant String := "call";
   Bridged_With      : constant String := "bridged_with";
   B_Leg             : constant String := "b_leg";
   Call_ID_S         : constant String := "call_Id";
   Channel           : constant String := "channel";
   Contact_ID        : constant String := "contact_id";
   Contacts          : constant String := "contacts";
   Description       : constant String := "description";
   Full_Name         : constant String := "full_name";
   ID                : constant String := "id";
   Inbound           : constant String := "inbound";
   Is_Human          : constant String := "is_human";
   OpenID            : constant String := "openid";
   OpenIDs           : constant String := "openids";
   Organization_ID   : constant String := "organization_id";
   Organization_List : constant String := "organization_list";
   Length            : constant String := "length";
   Name              : constant String := "name";
   Queue             : constant String := "queue";
   Rank              : constant String := "rank";
   State_S           : constant String := "state";
   Status            : constant String := "status";
   OK                : constant String := "ok";
   URI               : constant String := "uri";
   URL               : constant String := "url";
   User_S            : constant String := "user";
   Users_S           : constant String := "users";

end View;
