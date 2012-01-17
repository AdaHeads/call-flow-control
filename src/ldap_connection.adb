-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              LDAP_Connection                              --
--                                                                           --
--                                  BODY                                     --
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

with Ada.Task_Attributes;

package body LDAP_Connection is

   package Task_Association is new Ada.Task_Attributes
     (AWS.LDAP.Client.Directory, AWS.LDAP.Client.Null_Directory);

   ---------------------
   --  Get_Directory  --
   ---------------------

   function Get_Directory return AWS.LDAP.Client.Directory
   is
      use AWS.LDAP.Client;

      A_Directory : Directory;
   begin
      A_Directory := Task_Association.Value;

      if Is_Open (A_Directory) then
         return A_Directory;
      else
         A_Directory := Init (Host, Port);
         Bind (A_Directory, User_DN, Password);

         Task_Association.Set_Value (A_Directory);

         return A_Directory;
      end if;
   end Get_Directory;

end LDAP_Connection;
