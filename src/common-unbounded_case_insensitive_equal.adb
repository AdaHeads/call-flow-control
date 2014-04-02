with
  Ada.Characters.Handling;

function Common.Unbounded_Case_Insensitive_Equal
           (Left, Right : in Ada.Strings.Unbounded.Unbounded_String)
  return Boolean is
   use Ada.Characters.Handling, Ada.Strings.Unbounded;
begin
   return To_Lower (To_String (Left)) = To_Lower (To_String (Right));
end Common.Unbounded_Case_Insensitive_Equal;
