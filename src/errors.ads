-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                  Errors                                   --
--                                                                           --
--                                  SPEC                                     --
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

with Ada.Exceptions;

package Errors is

   Database_Error : exception;
   --  Is raised for ANY fatal database errors.

   GET_Parameter_Error : exception;
   --  Is raised if one or more GET parameters are missing/wrong or otherwise
   --  in bad condition.

   procedure Error_Handler
     (Message : in String);
   --  Log error messages to the Error trace.

   function Exception_Handler
     (Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String)
      return String;
   --  Log exception messages to the Error trace and returns a JSON String
   --  containing the exception.

end Errors;
