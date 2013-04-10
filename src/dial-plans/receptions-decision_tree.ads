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

with PBX.Call,
     Receptions.List_Of_Branches;

private
with Ada.Strings.Unbounded;

package Receptions.Decision_Tree is
   type Instance is tagged private;
   subtype Class is Instance'Class;

   function Create (Title     : in     String;
                    Branches  : in     Receptions.List_Of_Branches.Vector;
                    Fall_Back : in     String) return Instance;

   function Title (Item : in     Instance) return String;

   function Branch (Item : in     Instance;
                    Call : in     PBX.Call.Identification) return String;

   XML_Element_Name : constant String := "decision-tree";
private
   type Instance is tagged
      record
         Title     : Ada.Strings.Unbounded.Unbounded_String;
         Branches  : Receptions.List_Of_Branches.Vector;
         Fall_Back : Ada.Strings.Unbounded.Unbounded_String;
      end record;
end Receptions.Decision_Tree;
