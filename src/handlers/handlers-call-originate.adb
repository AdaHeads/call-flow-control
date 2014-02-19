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

with Model.Contact,
     Model.User,
     Model.Phone,
     Model.Peer,
     PBX,
     PBX.Action,
     Request_Utilities,
     Response.Templates,
     System_Messages,
     View;

package body Handlers.Call.Originate is
   use AWS.Status,
       System_Messages,
       View,
       Model;

   ----------------
   --  Callback  --
   ----------------

   function Callback return AWS.Response.Callback is
   begin
      return Generate_Response'Access;
   end Callback;

   -------------------------
   --  Generate_Response  --
   -------------------------

   function Generate_Response
     (Request : in AWS.Status.Data) return AWS.Response.Data is
      use Model.User;

      Context : constant String := Package_Name & ".Generate_Response";

      Context_Param : constant String :=
        Parameters (Request).Get (Context_String);

      User              : constant Model.User.Instance :=
        Request_Utilities.User_Of (Request);

      Not_Found : exception;
   begin

      if not User.Peer.Registered then
         raise Model.Peer.Peer_Not_Registered;
      end if;

      if Context_Param = "" then
         return Response.Templates.Bad_Parameters
              (Request       => Request,
               Response_Body => Description
                 ("Invalid context paramteter: " & Context_Param));
      end if;

      if Parameters (Request).Exist (Extension_String) then
         declare
            Extension_Param : constant String :=
              Parameters (Request).Get (Extension_String);
         begin
            System_Messages.Debug
              (Message => "Originating to arbitrary extension: "
               & Extension_Param & " in Context " & Context_Param & ".",
               Context => Context);

            PBX.Action.Originate
              (User        => User,
               Extension   => Extension_Param);
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
                 (Reception => R_ID,
                  Contact   => C_ID).Extension_Of (Phone_ID => P_ID);
            begin
               if Extension = Model.Phone.Null_Extension then
                  raise Not_Found with "No such extension";
               end if;
               PBX.Action.Originate
                 (User        => User,
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

      return Response.Templates.OK (Request);
exception
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
