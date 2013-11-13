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

with Ada.Exceptions;
with AWS.Session;

with PBX.Trace;
with Common;
with HTTP_Codes;
with ESL.Peer;
with Model.Agent,
     Response;

with System_Messages;

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

   ------------------
   --  Login_User  --
   ------------------

   function Login_User
     (Request : in AWS.Status.Data)
      return AWS.Response.Data is
      use AWS.Status;
      use AWS.Session;

      Context         : constant String := Package_Name & "Login_User";
      User_String     : String renames Parameters (Request).Get ("user");

      Response_Object : Response.Object := Response.Factory (Request);
      User_ID         : Natural         := 0;
      Session_ID      : constant AWS.Session.Id  := AWS.Status.Session (Request);
   begin

      System_Messages.Notify (Level   => System_Messages.Debug,
                              Message => "Current agent: " & Model.Agent.Agent_Of (Request => Request).To_JSON.Write);

      User_ID := Natural'Value (User_String);

      Set (SID   => Session_ID,
           Key   => "user_id",
           Value => User_ID);

      User_ID := Get (SID   => Session_ID,
                      Key   => "user_id");

      PBX.Trace.Information (Message => "Logged in user " & User_ID'Img,
                             Context => Context);

      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content (Null_JSON_String);

      return Response_Object.Build;

   exception
      when E : others =>
         PBX.Trace.Error
           (Message => Ada.Exceptions.Exception_Information (E),
            Context => "handlers.agent.login_user");

      return Response_Object.Build;
   end Login_User;
end Handlers.Agent;
