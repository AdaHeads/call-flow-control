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

with Ada.Strings.Unbounded;

with ESL.Reply,
     ESL.UUID,
     ESL.Command.Core,
     ESL.Command.Call_Management,
     ESL.Command.Miscellaneous;

with GNATCOLL.JSON;
with PBX.Trace;
with Model.Peer.List;
with System_Messages;

package body PBX.Action is
   use GNATCOLL.JSON;
   use type Call.Identification;
   use type ESL.Reply.Responses;

   --------------
   --  Bridge  --
   --------------

   procedure Bridge (Source      : in Call.Identification;
                     Destination : in Call.Identification) is
      Reply         : ESL.Reply.Instance;
      Bridge_Action : constant ESL.Command.Call_Management.Instance :=
        ESL.Command.Call_Management.UUID_Bridge
          (UUID       => Source,
           UUID_Other => Destination);
   begin
      PBX.Client.API (Bridge_Action, Reply);

      if Reply.Response /= ESL.Reply.OK then
         raise Call.Not_Found;
      end if;
   end Bridge;

   ------------
   -- Hangup --
   ------------

   procedure Hangup (ID : in Call.Identification) is
      Hangup_Action : constant ESL.Command.Call_Management.Instance :=
        ESL.Command.Call_Management.UUID_Kill (UUID => ID.Image);
      Reply : ESL.Reply.Instance;

   begin
      PBX.Client.API (Hangup_Action, Reply);

      if Reply.Response /= ESL.Reply.OK then
         raise Call.Not_Found;
      end if;
   end Hangup;

   ------------
   -- Logoff --
   ------------

   procedure Logoff is
   begin
      raise Program_Error with "Not implemented!";
   end Logoff;

   -----------------
   --  Originate  --
   -----------------

   procedure Originate (User      : in Model.User.Instance;
                        Extension : in String) is
      Context          : constant String := Package_Name & ".Originate";
      Originate_Action : constant ESL.Command.Call_Management.Instance :=
        ESL.Command.Call_Management.Originate
          (Call_URL  => User.Call_URI,
           Extension => Extension);
      Reply : ESL.Reply.Instance;

   begin
      PBX.Trace.Information (Message => "Sending:" &
                               String (Originate_Action.Serialize),
                             Context => Context);
      PBX.Client.API (Originate_Action, Reply);

      --  TODO: Add more elaborate parsing here to determine if the call
      --  really _isn't_ found.
      if Reply.Response /= ESL.Reply.OK then
         raise Error;
      end if;

      PBX.Trace.Debug
        (Message => "Sending Originate request with exten " &
           Extension,
         Context => Context,
         Level   => 1);
   end Originate;

   ----------
   -- Park --
   ----------

   procedure Park (Target  : in Call.Identification;
                   At_User : in User.Instance) is
      use ESL.Reply;

      Context     : constant String := Package_Name & ".Park";
      Park_Action : constant ESL.Command.Call_Management.Instance :=
        ESL.Command.Call_Management.UUID_Transfer
          (UUID        => Target,
           Destination => At_User.Parking_Lot_Identifier);
      Reply : ESL.Reply.Instance;

   begin
      PBX.Trace.Information (Message => "Sending:" &
                               String (Park_Action.Serialize),
                             Context => Context);
      PBX.Client.API (Park_Action, Reply);

      --  TODO: Add more elaborate parsing here to determine if the call
      --  really _isn't_ found.
      if Reply.Response /= ESL.Reply.OK then
         raise Call.Not_Found;
      end if;
   end Park;

   --------------
   -- Transfer --
   --------------

   procedure Transfer (Target  : in Call.Identification;
                       At_User : in User.Instance) is
      use ESL.Reply;

      Context     : constant String := Package_Name & ".Transfer";

      Transfer_Action : constant ESL.Command.Call_Management.Instance :=
        ESL.Command.Call_Management.UUID_Transfer
          (UUID        => Target,
           Destination => At_User.Call_URI);
      Reply : ESL.Reply.Instance;

   begin
      PBX.Trace.Information (Message => "Sending:" &
                               String (Transfer_Action.Serialize),
                             Context => Context);
      PBX.Client.API (Transfer_Action, Reply);

      if Reply.Response /= ESL.Reply.OK then
         raise PBX.Action.Error with "Transfer command failed: " &
           Reply.Response_Body;
      end if;
   end Transfer;

   -----------------------
   -- Update_Call_List  --
   -----------------------

   procedure Update_Call_List is
      use ESL.Command;

      Context               : constant String :=
        Package_Name & ".Update_Call_List";

      Reply                 : ESL.Reply.Instance;
      List_Channels_Action  : ESL.Command.Core.Instance :=
        ESL.Command.Core.Show (Report => "detailed_calls");
   begin
      List_Channels_Action.Set_Format (Format => ESL.Command.JSON);
      PBX.Client.API (List_Channels_Action, Reply);

      if Reply.Response = ESL.Reply.Error then
         raise Error with "Update_Channel_List failed";
      end if;

      declare
         JSON_Body : constant JSON_Value :=
           GNATCOLL.JSON.Read (Strm => Reply.Response_Body);
         Arr  : JSON_Array;
      begin

         if JSON_Body.Has_Field (Field => "rows") then
            Arr := JSON_Body.Get (Field => "rows");

            for I in 1 .. Length (Arr) loop
               declare
                  Call_JSON : JSON_Value renames Get (Arr => Arr, Index => I);
                  UUID      : constant Call.Identification :=
                    ESL.UUID.Create (Call_JSON.Get (Field => "uuid"));
               begin
                  Call.Create_And_Insert
                    (Inbound         =>
                       (if   Call_JSON.Get ("direction") = "inbound"
                        then True
                        else False),
                     ID           => UUID,
                     Reception_ID => Null_Reception_Identifier);
                  if Call_JSON.Get (Field => "application") = "fifo" then
                     Call.Get (Call => UUID).Change_State
                       (New_State => Call.Queued);
                  end if;
               end;
               System_Messages.Error
                 (Message => "Inserting existing call into" &
                    "invalid reception due to missing Information!",
                  Context => Context);
            end loop;
         end if;
      end;
   end Update_Call_List;

   ---------------------------
   --  Update_SIP_Peer_List --
   ---------------------------

   procedure Update_SIP_Peer_List is
      use Ada.Strings.Unbounded;
      use ESL.Command;

      Context               : constant String :=
        Package_Name & ".Update_SIP_Peer_List";

      Reply              : ESL.Reply.Instance;
      List_Peers_Action  : constant ESL.Command.Miscellaneous.Instance :=
        ESL.Command.Miscellaneous.List_Users;
      Position           : Natural := 0;

      type Peer_Packet_Items is array (Natural range <>) of Unbounded_String;

      function Get_Line (Item : in     String;
                         Last :    out Natural) return String;

      function Get_Line (Item : in     String;
                         Last :    out Natural) return String is
      begin
         Last := Item'First;
         for Position in Item'Range loop
            exit when Position = Item 'Last or Item (Position) = ASCII.LF;
            case Item (Position) is
            when others =>
               Last := Position;
            end case;
         end loop;

         --  TODO run a show registrations to assert the registration state.

         return Item (Item'First .. Last);
      end Get_Line;

      function Parse_Line (Item : in     String)
                           return Peer_Packet_Items;

      function Parse_Line (Item : in     String)
                           return Peer_Packet_Items is
         Offset    : Natural := Item'First;
         Key_Count : Natural := 0;
      begin
         --  Count number of elements.
         for Pos in Item'Range loop
            if Item (Pos) = '|' or Pos = Item'Last then
               Offset := Pos + 1;
               Key_Count := Key_Count + 1;
            end if;
         end loop;

         Offset := Item'First;

         declare
            Headers : Peer_Packet_Items (1 .. Key_Count);
            Index   : Natural := Headers'First;
         begin
            for Pos in Item'Range loop
               if Item (Pos) = '|' then
                  Headers (Index) :=
                    To_Unbounded_String (Item (Offset .. Pos - 1));
                  Offset := Pos + 1;
                  Index := Index + 1;
               elsif Pos = Item'Last then
                  Headers (Index) :=
                    To_Unbounded_String (Item (Offset .. Pos));
                  Offset := Pos + 1;
                  Index := Index + 1;
               end if;
            end loop;
            return Headers;
         end;
      end Parse_Line;

   begin
      PBX.Client.API (List_Peers_Action, Reply);

      if Reply.Response = ESL.Reply.Error then
         PBX.Trace.Error (Message => "Update failed!",
                          Context => Context);
      end if;

      Position := Reply.Response_Body'First;

      --  Header.
      declare
         Header_Line    : constant String := Get_Line
           (Item => Reply.Response_Body, Last => Position);
         Header : constant Peer_Packet_Items := Parse_Line (Header_Line);
         Peers  : Model.Peer.List.Instance;
      begin
         Position := Position + 2;
         while Position < Reply.Response_Body'Last loop
            declare
               Line : constant String := Get_Line
                 (Item => Reply.Response_Body
                    (Position .. Reply.Response_Body'Last),
                  Last => Position);
               Items : constant Peer_Packet_Items := Parse_Line (Line);
            begin
               if Items'Length = Header'Length then
                  declare
                     Peer : Model.Peer.Instance;
                     Node : constant JSON_Value := Create_Object;
                  begin

                     for I in Items'First + 1 .. Items'Last loop
                        Node.Set_Field (Field_Name => To_String (Header (I)),
                                        Field      => Items (I));
                     end loop;

                     Peer := Model.Peer.Create (User_ID => Items (Items'First),
                                                Values  => Node);

                     Peers.Put (New_Peer => Peer);

                  end;
               else
                  PBX.Trace.Debug (Message => "Skipping line " &  Line,
                                   Context => Context);

               end if;
               Model.Peer.List.Set_Singleton (Peers);
            end;
            Position := Position + 2;
         end loop;
      end;
   end Update_SIP_Peer_List;

end PBX.Action;
