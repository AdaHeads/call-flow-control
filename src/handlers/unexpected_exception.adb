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
with Response.Templates;

with System_Messages;

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

      Context : constant String := Package_Name &
        ".Unexpected_Exception_Handler";

      function Message
        return String;
      --  Build the message that is appended to the log and response object.

      use System_Messages;

      ---------------
      --  Message  --
      ---------------

      function Message
        return String
      is
      begin
         if Error.Fatal then
            return "Fatal error - "
              & "AWS slot number"
              & Positive'Image (Error.Slot)
              & " died.";
         end if;

         return "Error occured.";
      end Message;
   begin
      System_Messages.Critical_Exception
        (Message => Message,
         Event   => E,
         Context => Context);

      Answer := Response.Templates.Server_Error;
   end Unexpected_Exception_Handler;

end Unexpected_Exception;
