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

with HTTP_Codes;

package System_Message.Error is

   procedure Bad_Request_Parameter is new Log_And_Respond
     (Description => "conversion failed",
      Log_Trace   => Yolk.Log.Error,
      Status      => "bad request",
      Status_Code => HTTP_Codes.Bad_Request);

   procedure Bad_Call_ID_Key is new Log_And_Respond
     (Description => "call_id must be a valid natural integer",
      Log_Trace   => Yolk.Log.Error,
      Status      => "bad request",
      Status_Code => HTTP_Codes.Bad_Request);

   procedure Client_Error is new Log_And_Respond
     (Description => "",
      Log_Trace   => Yolk.Log.Error,
      Status      => "Client log entry",
      Status_Code => HTTP_Codes.No_Content);

   procedure Generic_Constraint_Error is new Log_And_Respond
     (Description => "a constraint was violated",
      Log_Trace   => Yolk.Log.Error,
      Status      => "bad request",
      Status_Code => HTTP_Codes.Bad_Request);

   procedure Missing_Request_Parameter is new Log_And_Respond
     (Description => "required request parameter missing",
      Log_Trace   => Yolk.Log.Error,
      Status      => "bad request",
      Status_Code => HTTP_Codes.Bad_Request);

   procedure Not_Found is new Responder
     (Description => "requested resource does not exist",
      Status      => "not found",
      Status_Code => HTTP_Codes.Not_Found);

end System_Message.Error;
