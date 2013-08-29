-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2013-, AdaHeads K/S                     --
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

with Ada.Characters.Latin_1,
     Ada.Strings.Unbounded;

with AWS.Parameters,
     AWS.Status;

with Handlers.OpenID,
     System_Message.Info;

package body Handlers.Users.Validate is
   function Generate_Response (Request : in AWS.Status.Data)
                              return AWS.Response.Data;

   function Callback return AWS.Response.Callback is
   begin
      return Generate_Response'Access;
   end Callback;

   function Generate_Response (Request : in AWS.Status.Data)
                              return AWS.Response.Data is
      function Format_Parameters return String;
      function Format_Parameters return String is
         use Ada.Characters, Ada.Strings.Unbounded;
         use AWS.Parameters, AWS.Status;
         Data   : constant AWS.Parameters.List := Parameters (Request);
         Result : Unbounded_String := Null_Unbounded_String;
      begin
         for Index in 1 .. Count (Data) loop
            Append (Result, String'(Data.Get_Name (Index)));
            Append (Result, ": ");
            Append (Result, String'(Data.Get_Value (Index)));
            Append (Result, Latin_1.LF);
         end loop;
         return To_String (Result);
      end Format_Parameters;
   begin
      System_Message.Info.OpenID_Log_In_Attempt
        (Message => Format_Parameters);

      return Handlers.OpenID.Validate (Request);
   end Generate_Response;

end Handlers.Users.Validate;
