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

with Response;
with System_Message.Critical;

package body Unexpected_Exception is

   ----------------
   --  Callback  --
   ----------------

   function Callback
     return AWS.Exceptions.Unexpected_Exception_Handler
   is
   begin
      return Unexpected_Exception_Handler'Access;
   end Callback;

   ----------------------------------
   -- Unexpected_Exception_Handler --
   ----------------------------------

   procedure Unexpected_Exception_Handler
     (E      : in     Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : in     AWS.Exceptions.Data;
      Answer : in out AWS.Response.Data)
   is
      pragma Unreferenced (Log);

      function Message
        return String;
      --  Build the message that is appended to the log and response object.

      use System_Message;

      Response_Object : Response.Object := Response.Factory (Error.Request);

      ---------------
      --  Message  --
      ---------------

      function Message
        return String
      is
      begin
         if Error.Fatal then
            return Response_Object.To_Debug_String
              & " - Fatal error - "
              & "AWS slot number"
              & Positive'Image (Error.Slot)
              & " died.";
         end if;

         return Response_Object.To_Debug_String;
      end Message;
   begin
      Critical.Unhandled_Exception
           (Event           => E,
            Message         => Message,
            Response_Object => Response_Object);

      Answer := Response_Object.Build;
   end Unexpected_Exception_Handler;

end Unexpected_Exception;
