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

package Response is
   pragma Pure (Response);

   Status_Text                 : constant String := "status";
   Bad_Parameters_Reponse_Text : constant String := "bad parameters";
   Not_Authorized_Reponse_Text : constant String := "not authorized";
   Not_Found_Reponse_Text      : constant String := "not found";
   OK_Reponse_Text             : constant String := "ok";
   Server_Error_Reponse_Text   : constant String := "unhandled exception";

end Response;
