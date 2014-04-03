with
  Ada.Characters.Handling;

function Common.Case_Insensitive_Equal (Left, Right : in String)
  return Boolean is
   use Ada.Characters.Handling;
begin
   return To_Lower (Left) = To_Lower (Right);
end Common.Case_Insensitive_Equal;
