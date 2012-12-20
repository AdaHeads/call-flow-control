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

with
  Ada.Command_Line,
  Ada.Text_IO;

procedure AMI.Channel_ID.Test is
   use Ada.Command_Line;

   procedure Check_Image_And_Value
     (Input  : in String;
      Output : in String)
   is
      use Ada.Text_IO;
   begin
      declare
         Item : constant Instance := Value (Input);
      begin
         if Image (Item) = Output then
            null;
         else
            Put_Line ("""" & Input & """ becomes """ & Image (Item) &
                        """ and not """ & Output & """.");
            Set_Exit_Status (Failure);
         end if;
      end;
   exception
      when others =>
         Put_Line ("Conversion of """ & Input & """ raised an exception.");
         Set_Exit_Status (Failure);
   end Check_Image_And_Value;
begin
   Set_Exit_Status (Success);

   Check_Image_And_Value (Input  => "SIP/0004F2060EB4-0000001b",
                          Output => "SIP/0004F2060EB4-0000001B");

   Check_Image_And_Value (Input  => "SIP/JSA-N900-00000037",
                          Output => "SIP/JSA-N900-00000037");

   Check_Image_And_Value (Input  => "Parked/SIP/JSA-N900-0000003a",
                          Output => "Parked/SIP/JSA-N900-0000003A");

   Check_Image_And_Value (Input  => "SIP/JSA-N900-00000035<zombie>",
                          Output => "<temporary>");

   Check_Image_And_Value (Input  => "Parked/SIP/JSA-N900-00000035<zombie>",
                          Output => "<temporary>");

   Check_Image_And_Value (Input  => "GSM/JSA-N900-00000033",
                          Output => "<temporary>");
end AMI.Channel_ID.Test;
