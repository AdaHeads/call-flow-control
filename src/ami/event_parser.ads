with Ada.Strings.Unbounded;
package Event_Parser is
   use Ada.Strings.Unbounded;
   type KeyValue is (Key, Value);
   type Event_List_Type is array (Integer range <>, KeyValue range <>)
     of Unbounded_String;

   function Parse (Event_Text : in Unbounded_String) return Event_List_Type;

private
   function CountLines (Event : in Unbounded_String) return Natural;

end Event_Parser;
