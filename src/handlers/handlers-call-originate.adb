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

with Model.Call,
     Model.Contact,
     Model.User,
     Model.Phone,
     Model.Peer,
     Model.Token,
     PBX,
     PBX.Action,
     Request_Utilities,
     Response.Templates,
     System_Messages,
     View,
     View.Call;

package body Handlers.Call.Originate is
   use System_Messages,
       View,
       Model;

   procedure Check_Extension (Item : in String);

   Extension_Not_Found : exception;
   function Extension (Request : in Black.Request.Instance;
                       Context : in Reception_Contact_Identifier;
                       Token   : in Model.Token.Instance)
                      return String;

   ----------------
   --  Callback  --
   ----------------

   function Callback return HTTP.Callback is
   begin
      return Generate_Response'Access;
   end Callback;

   -----------------------
   --  Check_Extension  --
   -----------------------

   procedure Check_Extension (Item : in String) is
   begin
      if Item'Length < 2 then
         raise Invalid_Extension with
           "Extension should be longer than 2 characters";
      end if;
   end Check_Extension;

   function Extension (Request : in Black.Request.Instance;
                       Context : in Reception_Contact_Identifier;
                       Token   : in Model.Token.Instance)
                      return String is
   begin
      if Request.Has_Parameter (Extension_String) then
         return Result : constant String :=
           Request.Parameter (Extension_String)
         do
            Check_Extension (Result);
         end return;
      elsif Request.Has_Parameter (Key => Phone_ID_String) then
         declare
            Phone : Phone_Identifier;
         begin
            Phone :=
              Phone_Identifier'Value (Request.Parameter (Phone_ID_String));

            return Result : constant String :=
              Model.Contact.Fetch
                (Reception_Contact => Context,
                 Auth_Token        => Token).Extension_Of (Phone)
            do
               if Result = Model.Phone.Null_Extension then
                  raise Extension_Not_Found
                    with "No such extension";
               end if;
            end return;
         end;
      else
         raise Constraint_Error
           with "Bad parameters";
      end if;
   end Extension;

   -------------------------
   --  Generate_Response  --
   -------------------------

   function Generate_Response (Request : Black.Request.Instance)
                               return Black.Response.Class is
      use Model.User;
      use Model.Call;

      Context : constant String := Package_Name & ".Generate_Response";

      User              : constant Model.User.Instance :=
        Request_Utilities.User_Of (Request);
      Token             : constant Model.Token.Instance :=
        Request_Utilities.Token_Of (Request);

      Origination_Context : Reception_Contact_Identifier;
      New_Call_ID         : Model.Call.Identification;
   begin

      if not User.Peer.Registered then
         raise Model.Peer.Peer_Not_Registered;
      end if;

      if not Request.Has_Parameter (Context_String) then
         return Response.Templates.Bad_Parameters
                  (Request       => Request,
                   Response_Body => Description
                                      (Message => "Missing required context " &
                                                  "parameter."));
      end if;

      Origination :
      declare
         Raw : constant String := Request.Parameter (Context_String);
      begin
         Origination_Context := Value (Raw);
      exception
         when Constraint_Error =>
            return Response.Templates.Bad_Parameters
              (Request       => Request,
               Response_Body => Description ("Malformed context: " & Raw));
      end Origination;

      User.Park_Current_Call;

      New_Call_ID := PBX.Action.Originate
        (Reception_ID => Origination_Context.Reception_ID,
         Contact_ID   => Origination_Context.Contact_ID,
         User         => User,
         Extension    => Extension (Request, Origination_Context, Token));

      return Response.Templates.OK
        (Request       => Request,
         Response_Body => View.Call.Call_Stub (Call_ID => New_Call_ID));
   exception
      when Extension_Not_Found =>
         return Response.Templates.Not_Found
           (Request       => Request,
            Response_Body =>
              Description ("Phone_ID " & Request.Parameter (Phone_ID_String) &
                             " not associated with <" &
                             Image (Origination_Context) & ">."));

      when Event : Invalid_Extension =>
         --  TODO: Log extension.
         System_Messages.Information
           (Message => User.Image & " tried to dial invalid extension.",
            Context => Context);
         return
           Response.Templates.Bad_Parameters (Request,
                                              Description (Event => Event));

      when PBX.Action.Error =>
         System_Messages.Critical
           (Message => "PBX failed to handle request.",
            Context => Context);
         return Response.Templates.Server_Error (Request);

      when Model.Peer.Peer_Not_Registered =>
         System_Messages.Error (Message => "User has no peer registered.",
                                Context => Context);
         return Response.Templates.Server_Error
           (Request       => Request,
            Response_Body => Description ("User has no peer registered"));

      when Event : others =>
         System_Messages.Critical_Exception
           (Event   => Event,
            Message => "Unhandled exception.",
            Context => Context);
         return Response.Templates.Server_Error (Request);

   end Generate_Response;

end Handlers.Call.Originate;
