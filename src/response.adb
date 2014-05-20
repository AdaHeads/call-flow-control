
package body Response is

   function Create (Status      : in Statuses;
                    Description : in String     := "";
                    With_Body   : in JSON_Value := Create_Object)
                    return Instance is
   begin
      return (Keep_Open    => <>,
              Status       => Status,
              Description  => To_Unbounded_String (Description),
              Current_Body => With_Body);
   end Create;

   function Image (Object : in Instance) return String is
   begin
      return Object.Status'Img & " : " & Object.Current_Body.Write;
   end Image;

   function To_JSON (Object : in Instance) return JSON_Value is
      JSON : constant JSON_Value := Create_Object;
   begin
      JSON.Set_Field ("description", Object.Description);
      JSON.Set_Field ("status", Object.Status'Img);
      JSON.Set_Field ("response", Object.Current_Body);

      return JSON;
   end To_JSON;

   function To_JSON_String (Object : in Instance) return String is
   begin
      return Object.To_JSON.Write;
   end To_JSON_String;

end Response;
