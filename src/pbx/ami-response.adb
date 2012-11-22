with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Common;
with System_Messages;

package body AMI.Response is
   use System_Messages;

   function Hash_Equivalent_Keys (Left, Right : in Action_ID_Type)
                                  return Boolean is
   begin
      return Left = Right;
   end Hash_Equivalent_Keys;

   function Hash_Function (Key : in Action_ID_Type)
                           return Ada.Containers.Hash_Type is
   begin
      return Action_ID_Type'Pos (Key);
   end Hash_Function;

--   function Image (List : in Response_List_Type.Map) return String is
--      package Latin_1 renames Ada.Characters.Latin_1;
--      Buffer : Ada.Strings.Unbounded.Unbounded_String;
--   begin
--      for Cursor in List.Iterate loop
--         Ada.Strings.Unbounded.Append
--           (Buffer,
--            Latin_1.LF &
--              "[" & Response_List_Type.Key (Cursor)'Img &
--              "] => ["  &
--              "]");
--      end loop;
--
--      return Ada.Strings.Unbounded.To_String (Buffer);
--   end Image;

   procedure Notify (Client : access Client_Type;
                     Packet : in     AMI.Parser.Packet_Type) is
      use Ada.Strings.Unbounded;
      Key      : Action_ID_Type := Null_Action_ID;
      Callback : Callback_Type;
   begin

      Key := Action_ID_Type'Value (To_String
        (Packet.Fields.Element
           (AMI.Parser.ActionID)));
      System_Messages.Notify
        (Debug, "AMI.Response.Notify: Dispatching : " & Key'Img);

      Callback := Reponse_List.Element (Key);

      Callback (Client, Packet); --  Make the callback call
      Reponse_List.Delete (Key); --  And delete the subscription.

   exception
      when Constraint_Error =>
         System_Messages.Notify
           (Information, "AMI.Response.Notify: Bad index : " & Key'Img);
      when Error : others =>
         System_Messages.Notify
           (Debug, "AMI.Response.Notify: Unexpected exception: ");
         System_Messages.Notify
           (Debug, Ada.Exceptions.Exception_Information (Error));
   end Notify;

   procedure Subscribe (Action_ID : in Action_ID_Type;
                        Callback  : in Callback_Type) is
   begin
      Reponse_List.Insert (Key => Action_ID, New_Item => Callback);
   end Subscribe;

   function Wait_For (Action_ID : in Action_ID_Type;
                      Timeout   : in Duration := 3.0) return Boolean is
      use Common;
      use type Ada.Calendar.Time;

      Deadline : constant Ada.Calendar.Time := Current_Time + Timeout;
   begin
      loop
         exit when Current_Time > Deadline;

         if not Reponse_List.Contains (Action_ID) then
            return True;
         end if;

         delay 0.1;
      end loop;
      return False;
   end Wait_For;
end AMI.Response;
