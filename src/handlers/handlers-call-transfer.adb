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
     Model.Transfer_Requests,
     PBX,
     PBX.Action,
     Response.Templates,
     System_Messages,
     View;

package body Handlers.Call.Transfer is
   use AWS.Status,
       System_Messages,
       View,
       Model;

   function Callback return AWS.Response.Callback is
   begin
      return Generate_Response'Access;
   end Callback;

   -------------------------
   --  Generate_Response  --
   -------------------------

   function Generate_Response (Request : in AWS.Status.Data)
                               return AWS.Response.Data
   is
      use Model.Call;

      Context : constant String := Package_Name & ".Generate_Response";

      Source          : Model.Call.Identification := Null_Identification;
      Destination     : Model.Call.Identification := Null_Identification;
   begin
      --  Check valitity of the call. (Will raise exception on invalid).
      Source      := Value (Parameters (Request).Get (Source_String));
      Destination := Value (Parameters (Request).Get (Destination_String));

      --  Sanity checks.
      if
        Source      = Null_Identification or
        Destination = Null_Identification
      then
         raise Model.Call.Invalid_ID;
      elsif
        Get (Source).State      /= Parked and
        Get (Destination).State /= Parked
      then
         System_Messages.Error
           (Message =>
              "Potential invalid state detected; trying to bridge a " &
              "non-parked call in an attended transfer. uuids: (" &
              Source.Image & ", " & Destination.Image & ")",
            Context => Context);
      end if;

      if not Get (Source).Is_Call then
         System_Messages.Error
           (Message => "Source is not a call: " & Source.Image,
            Context => Context);
      end if;

      if not Get (Destination).Is_Call then
         System_Messages.Error
           (Message => "Destination is not a call: " & Destination.Image,
            Context => Context);
      end if;

      System_Messages.Debug
        (Message => "Transferring " &
           Source.Image & " -> " &
           Destination.Image,
         Context => Context);
      Model.Transfer_Requests.Create (IDs => (ID1 => Source,
                                              ID2 => Destination));

      PBX.Action.Bridge (Source      => Source,
                         Destination => Destination);

      return Response.Templates.OK (Request);
   exception
      when Invalid_ID =>
         return Response.Templates.Bad_Parameters
           (Request       => Request,
            Response_Body => Description ("Invalid or no call ID supplied"));

      when Model.Call.Not_Found =>
         return Response.Templates.Not_Found
           (Request       => Request,
            Response_Body => Description ("No call found with ID " &
                Parameters (Request).Get ("source")));

      when E : others =>
         Model.Transfer_Requests.Decline (IDs => (ID1 => Source,
                                                  ID2 => Destination));

         System_Messages.Critical_Exception
           (Event           => E,
            Message         => "Transfer request failed.",
            Context => Context);
         return Response.Templates.Server_Error (Request);
   end Generate_Response;
end Handlers.Call.Transfer;
