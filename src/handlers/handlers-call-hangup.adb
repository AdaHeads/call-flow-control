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

with GNATCOLL.JSON;

with Model.Call,
     Model.User,
     PBX,
     PBX.Action,
     Request_Utilities,
     Response.Templates,
     System_Messages;

package body Handlers.Call.Hangup is

   function Is_Administrator (User : in Black.Request.Instance) return Boolean;
   function Owns (User : in Black.Request.Instance;
                  Call : in Model.Call.Identification) return Boolean;

   function Success (Request : in Black.Request.Instance;
                     Call_ID : in Model.Call.Identification)
                    return Black.Response.Instance;
   function Not_Owner_Of_Call (Request : in Black.Request.Instance;
                               Call_ID : in Model.Call.Identification)
                              return Black.Response.Instance;
   function Not_Found (Request : in Black.Request.Instance;
                       Call_ID : in Model.Call.Identification)
                      return Black.Response.Instance;

   ----------------
   --  Callback  --
   ----------------

   function Callback return HTTP.Callback is
   begin
      return Generate_Response'Access;
   end Callback;

   -------------------------
   --  Generate_Response  --
   -------------------------

   function Generate_Response (Request : Black.Request.Instance)
                              return Black.Response.Class is
      Context : constant String := Package_Name & ".Generate_Response";

      use Model.Call, Model.User;
   begin
      declare
         Call_ID : Model.Call.Identification renames
           Value (Black.Request.Parameter (Request => Request,
                                           Key     => Call_ID_String));
      begin
         if Is_Administrator (Request) or Owns (Request, Call_ID) then
            PBX.Action.Hangup (ID => Call_ID);

            return Success (Request, Call_ID);
         else
            return Not_Owner_Of_Call (Request, Call_ID);
         end if;
      exception
         when Model.Call.Not_Found =>
            return Not_Found (Request, Call_ID);
         when E : others =>
            System_Messages.Critical_Exception
              (Event   => E,
               Message => "Hangup request failed for call ID " & Call_ID.Image,
               Context => Context);
            return Response.Templates.Server_Error (Request);
      end;
   exception
      when E : others =>
         System_Messages.Critical_Exception
           (Event   => E,
            Message => "Hangup request failed.",
            Context => Context);
         return Response.Templates.Server_Error (Request);
   end Generate_Response;

   function Is_Administrator (User : in Black.Request.Instance)
                             return Boolean is
      use Model.User;
   begin
      return Request_Utilities.User_Of (User).Permissions (Administrator);
   end Is_Administrator;

   function Not_Found (Request : in Black.Request.Instance;
                       Call_ID : in Model.Call.Identification)
                      return Black.Response.Instance is
      use GNATCOLL.JSON, Response;
      Reply : constant JSON_Value := Create_Object;
   begin
      Reply.Set_Field (Status_Text,
                       Not_Found_Response_Text);
      Reply.Set_Field (Description_Text,
                       Call_ID.Image & " may already be hung up.");

      return Templates.Not_Found (Request       => Request,
                                  Response_Body => Reply);
   end Not_Found;

   function Not_Owner_Of_Call (Request : in Black.Request.Instance;
                               Call_ID : in Model.Call.Identification)
                              return Black.Response.Instance is
      use GNATCOLL.JSON, Response;
      Reply : constant JSON_Value := Create_Object;
   begin
      Reply.Set_Field (Status_Text,
                       Not_Authorized_Response_Text);
      Reply.Set_Field (Description_Text,
                       Call_ID.Image & " not assigned to you.");

      return Templates.Forbidden (Request       => Request,
                                  Response_Body => Reply);
   end Not_Owner_Of_Call;

   function Owns (User : in Black.Request.Instance;
                  Call : in Model.Call.Identification) return Boolean is
      use Model.Call, Request_Utilities;
   begin
      return User_Of (User).Identification = Get (Call).Assigned_To;
   end Owns;

   function Success (Request : in Black.Request.Instance;
                     Call_ID : in Model.Call.Identification)
                    return Black.Response.Instance is
      use GNATCOLL.JSON, Response;
      Reply : constant JSON_Value := Create_Object;
   begin
      Reply.Set_Field (Status_Text,
                       OK_Response_Text);
      Reply.Set_Field (Description_Text,
                       Call_ID.Image & " hung up successfully.");

      return Templates.OK (Request       => Request,
                           Response_Body => Reply);
   end Success;

end Handlers.Call.Hangup;
