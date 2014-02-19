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

with Ada.Strings.Fixed;
--  private with Model.Phone.Utilities;

package body Model.Phone is
   use Model;

   -----------
   --  "="  --
   -----------

   function "=" (Left, Right : in Instance) return Boolean is
   begin
      return Left.ID = Right.ID;
   end "=";

   ------------------------
   --  Create_From_JSON  --
   ------------------------

   function Create_From_JSON (JSON : in JSON_Value) return Instance is
   begin
      return Object : Instance do
         Object.ID := JSON.Get (Field => Phone_ID_Key);
         Object.Kind := Kinds'Value (JSON.Get (Field => Kind_Key));
         Object.Value := JSON.Get (Field => Value_Key);
      exception
         when others =>
            raise Constraint_Error with "Malformed JSON map.";
      end return;
   end Create_From_JSON;

   ------------------------
   --  Create_From_JSON  --
   ------------------------

   function Create_From_JSON (JSON : in JSON_Array) return Model.Phone.List is
   begin
      return List : Model.Phone.List do
         for I in 1 .. Length (JSON) loop
            declare
               Phone  : constant Model.Phone.Instance :=
                 Model.Phone.Create_From_JSON
                   (JSON => Get (Arr => JSON, Index => I));
            begin
               List.Append (Phone);
            end;
         end loop;
      end return;
   end Create_From_JSON;

   -------------
   --  Fetch  --
   -------------

--     function Fetch (Reception : in Reception_Identifier;
--                     Contact   : in Contact_Identifier;
--                     Phone     : in Phone_Identifier) return Instance is
--     begin
--           return Model.Phone.Utilities.Retrieve
--             (Reception => Reception,
--              Contact   => Contact,
--              From      => Config.Get (Key => Contact_Server));
--     end Fetch;

   -------------
   --  Image  --
   -------------

   function Image (Phones : List) return String is
      use Phone_Storage;

      Buffer : Unbounded_String;
   begin
      Append (Buffer, "[");
      for C in Phones.Iterate loop
         Append (Buffer, Element (C).Image & ", ");
      end loop;
      Append (Buffer, "]");

      return To_String (Buffer);
   end Image;

   -------------
   --  Image  --
   -------------

   function Image (Object : Instance) return String is
      use Ada.Strings.Fixed;

      Phone_ID : constant String := Trim (Source => Object.ID'Img,
                                          Side   => Ada.Strings.Left);
   begin
      return "ID:" & Phone_ID & " Kind: " & Object.Kind'Img
        & " Value: " & To_String (Object.Value);
   end Image;

end Model.Phone;
