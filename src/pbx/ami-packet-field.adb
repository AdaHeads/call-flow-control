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

package body AMI.Packet.Field is
   function Create (Key :   in AMI.Parser.AMI_Key_Type;
                    Value : in String) return Field is
   begin
      return (Key   => Key_String.To_Bounded_String (Key'Img),
              Value => Value_String.To_Bounded_String (Value));
   end Create;

   function Create (Key :   in String;
                    Value : in String) return Field is
   begin
      return (Key   => Key_String.To_Bounded_String (Key),
              Value => Value_String.To_Bounded_String (Value));
   end Create;

   function To_AMI_Line (F : in Field) return AMI_Line is
   begin
      return
        AMI_Line (Key_String.To_String (F.Key) & Separator &
                    Value_String.To_String (F.Value) &
                    Line_Termination_String);
   end To_AMI_Line;
end AMI.Packet.Field;
