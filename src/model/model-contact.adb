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
with Alice_Configuration;
with Protocol_Definitions;

private with Model.Contact.Utilities;

package body Model.Contact is
   use Alice_Configuration;
   use Model;
   use Protocol_Definitions;

   -----------
   --  "="  --
   -----------

   function "=" (Left, Right : in Instance) return Boolean is
   begin
      return Left.Reception_ID = Right.Reception_ID
        and  Left.Contact_ID   = Right.Contact_ID;
   end "=";

   ------------------------
   --  Create_From_JSON  --
   ------------------------

   function Create_From_JSON (JSON : in JSON_Array) return Phone_List is
   begin
      return List : Phone_List do
         for I in 1 .. Length (JSON) loop
            declare
               Number : constant String :=
                 Get (Get (Arr => JSON, Index => I), "value");
            begin
               List.Append (To_Unbounded_String (Number));
            end;
         end loop;
      end return;
   end Create_From_JSON;

   ------------------------
   --  Create_From_JSON  --
   ------------------------

   function Create_From_JSON (JSON : in JSON_Value) return Instance is
   begin
      return (Contact_ID => Contact_Identifier
              (Natural'(JSON.Get (Field => Contact_ID_Key))),
              Reception_ID => Reception_Identifier
                (Natural'(JSON.Get (Field => Reception_ID_Key))),
              Phones => Create_From_JSON (JSON.Get
                (Field => Telephone_Numbers_Key)));
   end Create_From_JSON;

   --------------------
   --  Extension_Of  --
   --------------------

   function Extension_Of (Object : Instance;
                          Phone  : Phone_Identifier) return String is
   begin
      return To_String (Object.Phones.Element (Phone));
   exception
      when Constraint_Error =>
         return Null_Extension;
   end Extension_Of;

   -------------
   --  Fetch  --
   -------------

   function Fetch (Reception : in Reception_Identifier;
                   Contact   : in Contact_Identifier) return Instance is
   begin
         return Model.Contact.Utilities.Retrieve
           (Reception => Reception,
            Contact   => Contact,
            From      => Config.Get (Key => Contact_Server));
   end Fetch;

   -------------
   --  Image  --
   -------------

   function Image (Phones : Phone_List) return String is
      use Phones_Storage;

      Buffer : Unbounded_String;
   begin
      Append (Buffer, "[");
      for C in Phones.Iterate loop
         Append (Buffer, Element (C) & ", ");
      end loop;
      Append (Buffer, "]");

      return To_String (Buffer);
   end Image;

   -------------
   --  Image  --
   -------------

   function Image (Object : Instance) return String is
      use Ada.Strings.Fixed;

      C_ID : constant String := Trim (Source => Object.Contact_ID'Img,
                                     Side   => Ada.Strings.Left);
      R_ID : constant String := Trim (Source => Object.Reception_ID'Img,
                                        Side   => Ada.Strings.Left);
   begin
      return C_ID & "@" & R_ID & " -> " & Image (Object.Phones);
   end Image;

end Model.Contact;
