with Ada.Containers.Indefinite_Hashed_Maps;
with Ada_2012.Strings.Hash_Case_Insensitive;
with Ada_2012.Strings.Equal_Case_Insensitive;
with Model.User;

with GNATCOLL.JSON;

with GNAT.Sockets;

package Request is
   use GNATCOLL.JSON;
   use GNAT.Sockets;

   Parse_Error : exception;

   Resource_Field   : constant String := "resource";
   Parameters_Field : constant String := "parameters";
   User_Field       : constant String := "user";
   subtype JSON_String is String;

   package Parameter_Storage is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada_2012.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada_2012.Strings.Equal_Case_Insensitive,
      "="             => Ada_2012.Strings.Equal_Case_Insensitive);

   type Resources is
     (Unknown,
      Call_List,  Call_Hangup,   Call_Originate,
      Call_Park,  Call_Pickup,   Call_Pickup_Next,
      Call_Queue, Call_Transfer,
      Peer_List,  Event_Socket);

   type Instance is tagged
      record
         Client     : Socket_Type := No_Socket;
         Resource   : Resources := Unknown;
         Parameters : Parameter_Storage.Map;
         User       : Model.User.Instance;
      end record;

   function Create (Client : in Socket_Type;
                    From   : in JSON_String) return Instance;

   function Parameter (Object : in Request.Instance;
                       Key    : in String) return String;

   function Image (Object : in Request.Instance) return String;

   function Image (Map : in Parameter_Storage.Map) return String;

   function Has_Parameter (Object : in Request.Instance;
                           Key    : in String) return Boolean;

   function Parameter (Object  : in Request.Instance;
                       Key     : in String;
                       Default : in String) return String;

end Request;
