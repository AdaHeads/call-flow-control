-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                  Errors                                   --
--                                                                           --
--                                  BODY                                     --
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

with Ada.Task_Identification;
with GNATCOLL.JSON;
with Yolk.Log;

package body Errors is

   ---------------------
   --  Error_Handler  --
   ---------------------

   procedure Error_Handler
     (Message : in String)
   is
      use Ada.Task_Identification;
      use Yolk.Log;

   begin
      Trace (Error,
             Image (Current_Task) & "-" & Message);
   end Error_Handler;

   -------------------------
   --  Exception_Handler  --
   -------------------------

   function Exception_Handler
     (Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String)
      return Common.JSON_String
   is
      use Ada.Exceptions;
      use Ada.Task_Identification;
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Log;

      E_Name : constant String     := Exception_Name (Event);
      E_Msg  : constant String     := Exception_Message (Event);
      JSON   : constant JSON_Value := Create_Object;
   begin
      Trace (Error,
             Image (Current_Task) &
             "-" & E_Name & "-" & E_Msg & "-" & Message);

      JSON.Set_Field (Field_Name => "exception",
                             Field      => E_Name);
      JSON.Set_Field (Field_Name => "exception_message",
                             Field      => E_Msg);
      JSON.Set_Field (Field_Name => "message",
                             Field      => Message);
      return To_JSON_String (JSON.Write);
   end Exception_Handler;

end Errors;
