with
  Ada.Characters.Handling,
  Ada.Strings.Hash;

function Common.Case_Insensitive_Hash (Key : in String)
  return Ada.Containers.Hash_Type is
   use Ada.Characters.Handling;
begin
   return Ada.Strings.Hash (To_Lower (Key));
end Common.Case_Insensitive_Hash;
