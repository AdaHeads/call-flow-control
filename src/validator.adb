-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                Validator                                  --
--                                                                           --
--                                  BODY                                     --
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

with AWS.Status;
with Common;
with Model;
with System_Message.Error;

package body Validator is

   function Parameter_Value
     (Name            : in Unbounded_String;
      Response_Object : in Response.Object)
      return String;
   --  TODO: Write comment

   procedure Validate
     (Value : in Model.Contact_Identifier)
   is null;

   procedure Validate
     (Value : in Model.Organization_Identifier)
   is null;

   -----------------------
   --  Parameter_Value  --
   -----------------------

   function Parameter_Value
     (Name            : in Unbounded_String;
      Response_Object : in Response.Object)
      return String
   is
      use AWS.Status;
   begin
      return Parameters (Response_Object.Status_Data).Get (To_String (Name));
   end Parameter_Value;

   ----------------
   --  Register  --
   ----------------

   procedure Register
     (Instance       : in out Object;
      Parameter_Name : in     String;
      Validate_As    : in     Value_Type)
   is
      use Common;
   begin
      Instance.Validations.Append ((U (Parameter_Name), Validate_As));
   end Register;

   ----------------
   --  Validate  --
   ----------------

   procedure Validate
     (Instance        : in     Object;
      Response_Object : in out Response.Object)
   is
      use AWS.Status;
      use Common;
      use System_Message;

      Set : Validation_Set;
   begin
      for Elem of Instance.Validations loop
         Set := Elem;

         case Elem.Validate_As is
            when Contact_Identifier =>
               Validate (Model.Contact_Identifier'Value
                         (Parameter_Value (Elem.Name, Response_Object)));
            when Organization_Identifier =>
               Validate (Model.Organization_Identifier'Value
                         (Parameter_Value (Elem.Name, Response_Object)));
         end case;
      end loop;

   exception
      when Constraint_Error =>
         Error.Bad_Request_Parameter
           (Message         => To_String (Set.Name)
            & " must be a valid "
            & Value_Type'Image (Set.Validate_As),
            Response_Object => Response_Object);
   end Validate;

end Validator;
