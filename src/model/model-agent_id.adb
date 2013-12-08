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
with Ada.Strings;
with Ada.Strings.Fixed;

package body Model.Agent_ID is

   -----------
   --  "<"  --
   -----------

   function "<" (Left  : in Agent_ID_Type;
                 Right : in Agent_ID_Type) return Boolean is
   begin
      return Left.ID < Right.ID;
   end "<";

   --------------
   --  Create  --
   --------------

   function Create (Agent_ID : in String) return Agent_ID_Type is
   begin
      return (ID => Natural'Value (Agent_ID));
   exception
      when Constraint_Error =>
         raise Invalid_ID with "Bad value: " & Agent_ID;
   end Create;

   --------------
   --  Create  --
   --------------

   function Create (Agent_ID : in Natural) return Agent_ID_Type is
   begin
      return (ID => Agent_ID);
   exception
      when Constraint_Error =>
         raise Invalid_ID with "Bad value:" & Agent_ID'Img;
   end Create;

   ------------
   --  Hash  --
   ------------

   function Hash (Agent_ID : Agent_ID_Type) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Agent_ID.ID);
   end Hash;

   ----------
   --  ID  --
   ----------

   function ID (Agent_ID : in Agent_ID_Type) return Natural is
   begin
      return Agent_ID.ID;
   end ID;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON (Obj : in Agent_ID_Type) return GNATCOLL.JSON.JSON_Value is
      use Ada.Strings;
   begin
      if Obj = Null_Agent_ID then
         return GNATCOLL.JSON.JSON_Null;
      else
         return GNATCOLL.JSON.Create
           (Ada.Strings.Fixed.Trim (Obj.ID'Img, Both));
      end if;
   end To_JSON;

   -----------------
   --  To_String  --
   -----------------

   function To_String (Agent_ID : in Agent_ID_Type) return String is
      use Ada.Strings;
   begin
      if Agent_ID = Null_Agent_ID then
         return "<null>";
      else
         return Ada.Strings.Fixed.Trim (Agent_ID.ID'Img, Both);
      end if;
   end To_String;

   ----------------
   --  Validate  --
   ----------------

   function Validate (Item : in String) return Boolean is
      Agent_ID : Agent_ID_Type := Null_Agent_ID;
      pragma Unreferenced (Agent_ID); --  Dummy variable
   begin
      if Item'Length < 1 then
         return False;
      end if;

      Agent_ID := Create (Agent_ID => Item);

      return True;

   exception
      when Invalid_ID =>
         return False;
   end Validate;

end Model.Agent_ID;
