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

with ESL.Reply;
with ESL.UUID;
with GNATCOLL.JSON;
with PBX.Trace;
with ESL.Command.Core;
with ESL.Command.Call_Management;

package body PBX.Action is
   use GNATCOLL.JSON;
   use type PBX.Call.Identification;
   use type ESL.Reply.Responses;

   --------------
   --  Bridge  --
   --------------

   procedure Bridge (Source      : in PBX.Call.Identification;
                     Destination : in PBX.Call.Identification) is
      use PBX.Call;

      Reply         : ESL.Reply.Instance;
      Bridge_Action : constant ESL.Command.Call_Management.Instance :=
        ESL.Command.Call_Management.UUID_Bridge
          (UUID       => Source,
           UUID_Other => Destination);
   begin
      PBX.Client.API (Bridge_Action, Reply);

      if Reply.Response /= ESL.Reply.OK then
         raise Not_Found;
      end if;
   end Bridge;

   ------------
   -- Hangup --
   ------------

   procedure Hangup (ID : in PBX.Call.Identification) is
      use PBX.Call;
      Hangup_Action : constant ESL.Command.Call_Management.Instance :=
        ESL.Command.Call_Management.UUID_Kill (UUID => ID.Image);
      Reply : ESL.Reply.Instance;

   begin
      PBX.Client.API (Hangup_Action, Reply);

      if Reply.Response /= ESL.Reply.OK then
         raise Not_Found;
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

   procedure Originate (Agent       : in Model.Agent.Agent_Type;
                        Extension   : in String) is
      Context          : constant String := Package_Name & ".Originate";
      Originate_Action : constant ESL.Command.Call_Management.Instance :=
        ESL.Command.Call_Management.Originate
          (Call_URL  => Agent.Peer_ID.To_String,
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

   procedure Park (Call  : in PBX.Call.Identification;
                   Agent : in Model.Agent.Agent_Type) is
      use PBX.Call;
      use ESL.Reply;

      Context     : constant String := Package_Name & ".Park";
      Park_Action : constant ESL.Command.Call_Management.Instance :=
        ESL.Command.Call_Management.UUID_Transfer
          (UUID        => Call,
           Destination => "park+" & Agent.ID.To_String);
      Reply : ESL.Reply.Instance;

   begin
      PBX.Trace.Information (Message => "Sending:" &
                               String (Park_Action.Serialize),
                             Context => Context);
      PBX.Client.API (Park_Action, Reply);

      --  TODO: Add more elaborate parsing here to determine if the call
      --  really _isn't_ found.
      if Reply.Response /= ESL.Reply.OK then
         raise Not_Found;
      end if;
   end Park;

   --------------
   -- Transfer --
   --------------

   procedure Transfer (Call  : in PBX.Call.Identification;
                       Agent : in Model.Agent.Agent_Type) is
      use PBX.Call;
      use ESL.Reply;

      Context     : constant String := Package_Name & ".Transfer";

      Transfer_Action : constant ESL.Command.Call_Management.Instance :=
        ESL.Command.Call_Management.UUID_Transfer
          (UUID        => Call,
           Destination => Agent.Extension);
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
      pragma Unreferenced (Context);

      Reply                 : ESL.Reply.Instance;
      List_Channels_Action  : ESL.Command.Core.Instance :=
        ESL.Command.Core.Show (Report => "detailed_calls");
   begin
      List_Channels_Action.Set_Format (Format => JSON);
      PBX.Client.API (List_Channels_Action, Reply);

      if Reply.Response = ESL.Reply.Error then
         raise Error with "Update_Channel_List failed";
      end if;

      declare
         JSON : constant JSON_Value :=
           GNATCOLL.JSON.Read (Strm => Reply.Response_Body);
         Arr  : JSON_Array;
      begin

         if JSON.Has_Field (Field => "rows") then
            Arr := JSON.Get (Field => "rows");

            for I in 1 .. Length (Arr) loop
               declare
                  Call_JSON : JSON_Value renames Get (Arr => Arr, Index => I);
                  UUID      : constant PBX.Call.Identification :=
                    ESL.UUID.Create (Call_JSON.Get (Field => "uuid"));
               begin
                  PBX.Call.Create_And_Insert
                    (Inbound         =>
                       (if   Call_JSON.Get ("direction") = "inbound"
                        then True
                        else False),
                     ID              => UUID);
                  if Call_JSON.Get (Field => "application") = "fifo" then
                     PBX.Call.Get (Call => UUID).Change_State
                       (New_State => PBX.Call.Queued);
                  end if;
               end;

            end loop;
         end if;
      end;
   end Update_Call_List;

   ---------------------------
   --  Update_SIP_Peer_List --
   ---------------------------

   procedure Update_SIP_Peer_List is

   begin
      raise Program_Error with "Not implemented!";
   end Update_SIP_Peer_List;

end PBX.Action;
