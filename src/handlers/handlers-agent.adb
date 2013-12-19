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

with Common;
with HTTP_Codes;
with ESL.Peer;
with Model.Agent,
     Response;

package body Handlers.Agent is
   use ESL;
   use Common;
   use HTTP_Codes;

   -------------
   --  Agent  --
   -------------

   function Agent
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      JSON            : JSON_String;
      Response_Object : Response.Object := Response.Factory (Request);
   begin
      JSON := To_JSON_String (Peer.List.To_JSON);

      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content (JSON);

      return Response_Object.Build;
   end Agent;

   ------------------
   --  Agent_List  --
   ------------------

   function Agent_List
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is

      Response_Object : Response.Object := Response.Factory (Request);
   begin
      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content (To_JSON_String (Model.Agent.To_JSON));

      return Response_Object.Build;
   end Agent_List;

end Handlers.Agent;
