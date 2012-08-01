with Ada.Containers.Ordered_Maps,
     Ada.Strings.Unbounded;

package Event_Parser is
   use Ada.Strings.Unbounded;

   package Event_List_Type is new Ada.Containers.Ordered_Maps
     (Unbounded_String, Unbounded_String);

   --  Takes a line of text, with key value pairs structured:
   --  Key: Value<CRLF>
   function Parse (Event_Text : in Unbounded_String)
                   return Event_List_Type.Map;

end Event_Parser;
