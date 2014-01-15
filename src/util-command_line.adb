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

with Ada.Command_Line;
with Ada.Strings.Fixed;

package body Util.Command_Line is

   -----------
   --  Get  --
   -----------

   function Get
     (Parameter : in String;
      Default   : in String := "")
      return String
   is
      use Ada.Command_Line;
   begin
      for K in 1 .. Argument_Count - 1 loop
         if Argument (K) = Parameter then
            return Argument (K + 1);
         end if;
      end loop;

      return Default;
   end Get;

   -----------
   --  Get  --
   -----------

   function Get
     (Parameter : in String;
      Prefix    : in String := "--")
      return String_Vectors.Vector
   is
      use Ada.Command_Line;
      use Ada.Strings.Fixed;

      Collecting : Boolean := False;
   begin
      return Arguments : String_Vectors.Vector do
         for Position in 1 .. Argument_Count loop
            if Argument (Position) = Parameter then
               Collecting := True;
            elsif Head (Argument (Position), Prefix'Length) = Prefix then
               Collecting := False;
            elsif Collecting then
               Arguments.Append (Argument (Position));
            end if;
         end loop;
      end return;
   end Get;

   function Got_Argument (Parameter : in String) return Boolean is
      use Ada.Command_Line;
   begin
      for K in 1 .. Argument_Count loop
         if Argument (K) = Parameter then
            return True;
         end if;
      end loop;

      return False;
   end Got_Argument;

end Util.Command_Line;
