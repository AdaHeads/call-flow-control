-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
--                     Author: Kim Rostgaard Christensen                     --
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

with Model.Call;

package Model.Origination_Requests is

   Package_Name : constant String := "Model.Origination_Requests";

   function Is_Origination_Request (ID : in Model.Call.Identification)
                                    return Boolean;

   procedure Create (ID : in Model.Call.Identification);

   procedure Confirm (ID : in Model.Call.Identification);

   procedure Decline (ID : in Model.Call.Identification);

end Model.Origination_Requests;
