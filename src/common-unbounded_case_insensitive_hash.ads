with
  Ada.Containers,
  Ada.Strings.Unbounded;

function Common.Unbounded_Case_Insensitive_Hash
           (Key : in Ada.Strings.Unbounded.Unbounded_String)
  return Ada.Containers.Hash_Type;
