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
     Model.User,
     PBX,
     PBX.Action,
     Request_Utilities,
     Response.Templates,
     System_Messages;

package body Handlers.Call.Hangup is

   ----------------
   --  Callback  --
   ----------------

   function Callback return Black.Response.Callback is
   begin
      return Generate_Response'Access;
   end Callback;

   -------------------------
   --  Generate_Response  --
   -------------------------

   function Generate_Response (Request : Black.Request.Instance)
                               return Black.Response.Instance is
      use Model.User;
      use Black.Request;

      Context : constant String := Package_Name & ".Generate_Response";

      Requested_Call_ID : String renames
        Parameters (Request).Get (Call_ID_String);
   begin
      --  An Administrator is able to end any call.
      if Request_Utilities.User_Of (Request).Permissions (Administrator) then
         PBX.Action.Hangup (ID => Model.Call.Value
                            (Item => Requested_Call_ID));
      else
         --  TODO: Add a way of verifying that the calling user owns the call.
         PBX.Action.Hangup (ID => Model.Call.Value
                            (Item => Requested_Call_ID));

      end if;

      return Response.Templates.OK (Request);

   exception
      when Model.Call.Not_Found =>
         return Response.Templates.Not_Found (Request);

      when E : others =>
         System_Messages.Critical_Exception
           (Event           => E,
            Message         => "Hangup request failed for call id: " &
              Requested_Call_ID,
            Context => Context);
         return Response.Templates.Server_Error (Request);
   end Generate_Response;

end Handlers.Call.Hangup;
