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
   use AWS.Status,
       System_Messages,
       View,
       Model;

   type Origination_Contexts is
      record
         Contact_ID   : Model.Contact_Identifier;
         Reception_ID : Model.Reception_Identifier;
      end record;

   function Create (Item : in String) return Origination_Contexts;

   procedure Check_Extension (Item : in String);

   ----------------
   --  Callback  --
   ----------------

   function Callback return AWS.Response.Callback is
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

   --------------
   --  Create  --
   --------------

   function Create (Item : in String) return Origination_Contexts is
      Sep_Pos : Natural := Item'First;
   begin
      return Value : Origination_Contexts do
         for Index in Item'Range loop
            case Item (Index) is
               when '@' =>
                  if Index /= Item'First then
                     Value.Contact_ID :=
                       Model.Contact_Identifier'Value
                         (Item (Item'First .. Index - 1));
                  end if;
                  Sep_Pos   :=  Index;
               when '0' .. '9' =>
                  null;
               when others =>
                  raise Constraint_Error with "Invalid character " &
                    Item (Index);
            end case;
         end loop;

         Value.Reception_ID :=
           Model.Reception_Identifier'Value
             (Item (Sep_Pos + 1 .. Item'Last));
      end return;
   end Create;

   -------------------------
   --  Generate_Response  --
   -------------------------

   function Generate_Response
     (Request : in AWS.Status.Data) return AWS.Response.Data is
      use Model.User;
      use Model.Call;

      Context : constant String := Package_Name & ".Generate_Response";

      Context_Param : constant String :=
        Parameters (Request).Get (Context_String);

      User              : constant Model.User.Instance :=
        Request_Utilities.User_Of (Request);
      Token             : constant Model.Token.Instance :=
        Request_Utilities.Token_Of (Request);

      Not_Found : exception;
      Origination_Context : Origination_Contexts;

      New_Call_ID : Model.Call.Identification;
   begin

      if not User.Peer.Registered then
         raise Model.Peer.Peer_Not_Registered;
      end if;

      if Context_Param = "" then
         return Response.Templates.Bad_Parameters
              (Request       => Request,
               Response_Body => Description
                 ("Missing required context paramteter."));
      end if;

      Origination_Context := Create (Item => Context_Param);

      if Parameters (Request).Exist (Extension_String) then
         declare
            Extension_Param : constant String :=
              Parameters (Request).Get (Extension_String);
         begin

            Check_Extension (Extension_Param);

            --  If there is an active call, park it.
            User.Park_Current_Call;

            New_Call_ID := PBX.Action.Originate
              (Reception_ID => Origination_Context.Reception_ID,
               Contact_ID   => Origination_Context.Contact_ID,
               User         => User,
               Extension    => Extension_Param);
         end;
      elsif Parameters (Request).Exist (Phone_ID_String) then
         declare
            R_ID : Reception_Identifier;
            C_ID : Contact_Identifier;
            P_ID_Param : constant String :=
              Parameters (Request).Get (Phone_ID_String);
            P_ID : Phone_Identifier;
         begin
            R_ID :=  Reception_Identifier'Value ("" & Context_Param (3));
            C_ID := Contact_Identifier'Value ("" & Context_Param (1));
            P_ID := Phone_Identifier'Value (P_ID_Param);

            declare
               Extension : constant String := Model.Contact.Fetch
                 (Reception  => R_ID,
                  Contact    => C_ID,
                  Auth_Token => Token).Extension_Of (Phone_ID => P_ID);
            begin
               if Extension = Model.Phone.Null_Extension then
                  raise Not_Found with "No such extension";
               end if;

               --  If there is an active call, park it.
               User.Park_Current_Call;

               New_Call_ID := PBX.Action.Originate
                 (Reception_ID => Origination_Context.Reception_ID,
                  Contact_ID   => Origination_Context.Contact_ID,
                  User         => User,
                  Extension   => Extension);
            end;
         exception
            when Not_Found =>
               return Response.Templates.Not_Found
                 (Request       => Request,
                  Response_Body => Description
                    ("Phone_ID " & P_ID_Param  &
                       " Not associated with:" & Context_Param));

            when Constraint_Error =>
               return Response.Templates.Bad_Parameters
                 (Request       => Request,
               Response_Body => Description
                    ("Malformed context: " & Context_Param));
         end;
      else
         raise Constraint_Error with "Bad parameters";
      end if;

      return Response.Templates.OK
        (Request       => Request,
         Response_Body => View.Call.Call_Stub (Call_ID => New_Call_ID));
   exception
      when Event : Invalid_Extension =>
         System_Messages.Information
           (Message         => "Tried to dial invalid extension: """ &
              Extension_String & "",
            Context => Context);
         return Response.Templates.Bad_Parameters
           (Request, Description (Event => Event));

      when PBX.Action.Error =>
         System_Messages.Critical
           (Message         => "PBX failed to handle request.",
            Context => Context);
         return Response.Templates.Server_Error (Request);

      when Model.Peer.Peer_Not_Registered =>
         System_Messages.Error
           (Message         => "User has no peer registered.",
            Context => Context);
         return Response.Templates.Server_Error
           (Request       => Request,
            Response_Body => View.Description
              (Message => "User has no peer registered"));

      when Event : others =>
         System_Messages.Critical_Exception
           (Event           => Event,
            Message         => "Unhandled exception.",
            Context => Context);
         return Response.Templates.Server_Error (Request);

   end Generate_Response;
end Handlers.Call.Originate;
