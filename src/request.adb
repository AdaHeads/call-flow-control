with Ada.Strings.Unbounded;

package body Request is
   use Ada.Strings.Unbounded;

   function Create (Client : in Socket_Type;
                    From   : in JSON_Value) return Instance;

   function Create (Client : in Socket_Type;
                    From   : in JSON_String) return Instance is
   begin
      return Create (Client => Client,
                     From   => Read (From));
   exception
      when others =>
         raise Parse_Error;
   end Create;

   function Create (Client : in Socket_Type;
                    From   : in JSON_Value) return Instance is
      Parameters : Parameter_Storage.Map;
   begin
      declare

         procedure Insert (Name : UTF8_String; Value : JSON_Value);

         procedure Insert (Name : UTF8_String; Value : JSON_Value) is
            pragma Unreferenced (Value);
         begin
            Parameters.Insert (Key      => Name,
                               New_Item => From.Get (Parameters_Field).
                                 Get (Name));
         end Insert;

      begin
         From.Get (Parameters_Field).Map_JSON_Object (CB => Insert'Access);
      end;

      return (Client     => Client,
              Resource   =>
                Resources'Value (From.Get (Field => Resource_Field)),
              Parameters => Parameters,
              User       => Model.User.Create
                                (From.Get (Field => User_Field)));
   exception
      when others =>
         raise Parse_Error;
   end Create;

   function Has_Parameter (Object : in Request.Instance;
                           Key    : in String) return Boolean is
   begin
      return Object.Parameters.Contains (Key => Key);
   end Has_Parameter;

   function Image (Object : in Request.Instance) return String is
   begin
      return Object.Resource'Img & " : " & Image (Object.Parameters) & " : " &
        Object.User.Image;
   end Image;

   function Image (Map : in Parameter_Storage.Map) return String is
      use Parameter_Storage;
      Buffer : Unbounded_String;
      C      : Cursor := Map.First;
   begin
      if C /= No_Element then
         Append (Buffer, Key (C) & " => " & Element (C));
         C := Next (C);
      end if;

      while C /= No_Element loop
         Append (Buffer, ", " & Key (C) & " => " & Element (C));
         C := Next (C);
      end loop;

      return  To_String (Buffer);
   end Image;

   function Parameter (Object : in Request.Instance;
                       Key    : in String) return String is
   begin
      return Object.Parameters.Element (Key => Key);
   end Parameter;

   function Parameter (Object  : in Request.Instance;
                       Key     : in String;
                       Default : in String) return String is
   begin
      return Object.Parameter (Key => Key);
   exception
      when Constraint_Error =>
         return Default;
   end Parameter;
end Request;
