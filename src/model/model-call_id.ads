package Model.Call_ID is
--  TODO: Make limited
--  TODO: Associate with channel_id in some spectacular fashion.
   type Call_ID_Type is tagged record
      Timestamp : Integer;
      Sequence  : Integer;
   end record;

   function Create (Item : in String) return Call_ID_Type;
   --  Constructor.

   function To_String (Call_ID : in Call_ID_Type) return String;

   function "<" (Left  : in Call_ID_Type;
                 Right : in Call_ID_Type) return Boolean;

   function "=" (Left  : in Call_ID_Type;
                 Right : in Call_ID_Type) return Boolean;

   Null_Call_ID : constant Call_ID_Type := (-1, -1);
end Model.Call_ID;
