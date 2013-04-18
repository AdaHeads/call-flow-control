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

with
  Ada.Containers.Indefinite_Hashed_Maps,
  Ada.Strings.Hash;
with
  Receptions.Dial_Plan,
  Receptions.Dial_Plan.IO;
with
  Phone_Numbers,
  SQL_Prepared_Statements.Dial_Plans,
  Storage;

package body Model.Dial_Plans is
   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Receptions.Dial_Plan.Class,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => Phone_Numbers.Same,
      "="             => Receptions.Dial_Plan."=");

   function To_Map (C : in out Database_Cursor'Class) return Maps.Map;

   function To_Dial_Plan (C : in out Database_Cursor'Class)
                         return Receptions.Dial_Plan.Class;

   procedure Number_And_Dial_Plan is
     new Storage.Process_Select_Query (Element           => Maps.Map,
                                       Database_Cursor   => Database_Cursor,
                                       Cursor_To_Element => To_Map);

   procedure Dial_Plan_Only is
      new Storage.Process_Select_Query
            (Element           => Receptions.Dial_Plan.Class,
             Database_Cursor   => Database_Cursor,
             Cursor_To_Element => To_Dial_Plan);

   Loaded : Maps.Map;

   procedure Clear is
   begin
      Loaded.Clear;
   end Clear;

   function End_Point (Number : in     String;
                       Call   : in     Receptions.PBX_Interface.Call'Class)
     return Receptions.End_Point.Class is
   begin
      return Loaded.Element (Number).Application (Call);
   end End_Point;

   procedure Load (Number : in     String) is
      Found : Natural := 0;

      procedure Insert (Item : in    Receptions.Dial_Plan.Class);
      procedure Insert (Item : in    Receptions.Dial_Plan.Class) is
      begin
         Found := Found + 1;
         Loaded.Insert (Key      => Number,
                        New_Item => Item);
      exception
         when Constraint_Error =>
            Loaded.Replace (Key      => Number,
                            New_Item => Item);
      end Insert;

      Normalised_Phone_Number : aliased constant String :=
                                  Phone_Numbers.Normalise (Number);

      use GNATCOLL.SQL.Exec;
   begin
      Dial_Plan_Only
        (Process_Element    => Insert'Access,
         Prepared_Statement => SQL_Prepared_Statements.Dial_Plans.Get_Specific,
         Query_Parameters   => (1 => +Normalised_Phone_Number'Access));

      case Found is
         when 0      => raise Not_Found;
         when 1      => null;
         when others => raise Constraint_Error;
      end case;
   end Load;

   procedure Reload_All is
      procedure Insert (Item : in    Maps.Map);
      procedure Insert (Item : in    Maps.Map) is
         Result : constant Maps.Cursor := Item.First;
      begin
         Loaded.Insert (Key      => Maps.Key (Result),
                        New_Item => Maps.Element (Result));
      end Insert;

      use GNATCOLL.SQL.Exec;
   begin
      Loaded.Clear;

      Number_And_Dial_Plan
        (Process_Element    => Insert'Access,
         Prepared_Statement => SQL_Prepared_Statements.Dial_Plans.Get_All);
   end Reload_All;

   function To_Dial_Plan (C : in out Database_Cursor'Class)
                         return Receptions.Dial_Plan.Class is
   begin
      return Receptions.Dial_Plan.IO.From_XML (Value (C, 0));
   end To_Dial_Plan;

   function To_Map (C : in out Database_Cursor'Class) return Maps.Map is
   begin
      return Result : Maps.Map do
         Result.Insert
           (Key      => Value (C, 0),
            New_Item => Receptions.Dial_Plan.IO.From_XML (Value (C, 1)));
      end return;
   end To_Map;

end Model.Dial_Plans;
