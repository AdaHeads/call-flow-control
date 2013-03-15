-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2012-, AdaHeads K/S                    --
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

package body Receptions.Dial_Plan is
   function Application (Item : in     Instance;
                         Call : in     PBX.Call.Identification)
     return Receptions.End_Point.Class is

      function "+" (Item : in Ada.Strings.Unbounded.Unbounded_String)
        return String
        renames Ada.Strings.Unbounded.To_String;
      function "+" (Item : in String)
        return Ada.Strings.Unbounded.Unbounded_String
        renames Ada.Strings.Unbounded.To_Unbounded_String;

      Next_Action : Ada.Strings.Unbounded.Unbounded_String := Item.Start_At;
   begin
      for Jumps in 0 .. Item.Decision_Trees.Length loop
         if Item.End_Points.Contains (+Next_Action) then
            return Item.End_Points.Element (+Next_Action);
         elsif Item.Decision_Trees.Contains (+Next_Action) then
            Next_Action := +Item.Decision_Trees.Element
                              (+Next_Action).Branch (Call);
         else
            raise Dead_End;
         end if;
      end loop;

      raise Circular;
   end Application;

   function Create
     (Title          : in     String;
      Start_At       : in     String;
      End_Points     : in     Receptions.End_Point_Collection.Map;
      Decision_Trees : in     Receptions.Decision_Tree_Collection.Map)
     return Instance is
      use Ada.Strings.Unbounded;
   begin
      return (Title          => To_Unbounded_String (Title),
              Start_At       => To_Unbounded_String (Start_At),
              End_Points     => End_Points,
              Decision_Trees => Decision_Trees);
   exception
      when E : Constraint_Error =>
         raise Constraint_Error with "Receptions.Dial_Plan.Create: " &
                                     Ada.Exceptions.Exception_Message (E);
   end Create;

   function Title (Item : in     Instance) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Title);
   end Title;
end Receptions.Dial_Plan;
