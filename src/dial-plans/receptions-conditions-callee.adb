-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2013-, AdaHeads K/S                    --
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

with System_Message.Info;

with Ada.Exceptions, Ada.Text_IO; use Ada.Exceptions, Ada.Text_IO;

package body Receptions.Conditions.Callee is
   not overriding
   function Create (Number : in String) return Instance is
      use Ada.Strings.Unbounded;
   begin
      return (Number => To_Unbounded_String (Number));
   exception
      when E : others =>
         Put_Line (File => Standard_Error,
                   Item => "Receptions.Conditions.Callee.Create raised " &
                           Exception_Name (E) & " with " &
                           Exception_Message (E) & ".");
         raise;
   end Create;

   overriding
   function True (Item : in Instance;
                  Call : in PBX.Call.Identification) return Boolean is
      use Ada.Strings.Unbounded,
          PBX.Call;
   begin
      System_Message.Info.Jacob_Wants_To_See_This
        (Message => "Actual callee: """ & Image (B_Leg (Get (Call))) &
                    """. Checking for callee: """ & To_String (Item.Number) &
                    """.");
      return Image (B_Leg (Get (Call))) = To_String (Item.Number);
   end True;

   overriding
   function Value (Item : in Instance) return String is
   begin
      return
        "Callee = """ & Ada.Strings.Unbounded.To_String (Item.Number) & """";
   end Value;
end Receptions.Conditions.Callee;
