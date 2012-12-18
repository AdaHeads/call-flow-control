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

with AWS.Messages;

package HTTP_Codes is

   OK           : constant AWS.Messages.Status_Code := AWS.Messages.S200;
   No_Content   : constant AWS.Messages.Status_Code := AWS.Messages.S204;

   Bad_Request  : constant AWS.Messages.Status_Code := AWS.Messages.S400;
   Unauthorized : constant AWS.Messages.Status_Code := AWS.Messages.S401;
   Forbidden    : constant AWS.Messages.Status_Code := AWS.Messages.S403;
   Not_Found    : constant AWS.Messages.Status_Code := AWS.Messages.S404;

   Internal_Server_Error : constant AWS.Messages.Status_Code
     := AWS.Messages.S500;
   Server_Error : constant AWS.Messages.Status_Code := Internal_Server_Error;

end HTTP_Codes;
