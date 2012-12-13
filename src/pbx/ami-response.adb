-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                     --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

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


   procedure Notify (Client : access Client_Type;
                     Packet : in     AMI.Parser.Packet_Type) is
      use Ada.Strings.Unbounded;
      Key      : Action_ID_Type := Null_Action_ID;
      Response : AMI.Packet.Action.Response_Handler_Type :=
                   AMI.Packet.Action.Null_Reponse_Handler'Access;
   begin

      Key := Action_ID_Type'Value (To_String
        (Packet.Fields.Element
           (AMI.Parser.ActionID)));
      System_Messages.Notify
        (Debug, "AMI.Response.Notify: Dispatching : " & Key'Img);

      Response := Reponse_List.Element (Key);

      Response (Packet); --  Make the callback call
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

   procedure Subscribe (Reply_For : in Request) is
   begin
      Reponse_List.Insert (Key      => Reply_For.Action_ID,
                           New_Item => Reply_For.Response_Handler);
   end Subscribe;


   procedure Wait_For (Action_ID : in Action_ID_Type;
                       Timeout   : in Duration := 3.0) is
      use Common;
      use type Ada.Calendar.Time;

      Deadline : constant Ada.Calendar.Time := Current_Time + Timeout;
   begin
      loop
         exit when Current_Time > Deadline;

         if not Reponse_List.Contains (Action_ID) then
            return;
         end if;

         delay 0.1;
      end loop;
      raise AMI.Response.Timeout;
   end Wait_For;
end AMI.Response;
