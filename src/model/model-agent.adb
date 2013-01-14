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

with Ada.Containers.Ordered_Maps;

package body Model.Agent is

   --  TODO: Change this to a real database.
   package Agent_Storage is new
     Ada.Containers.Ordered_Maps (Key_Type     => Agent_ID_Type,
                                  Element_Type => Agent_Type);

   Agent_List : Agent_Storage.Map := Agent_Storage.Empty_Map;

   --------------
   --  Assign  --
   --------------

   procedure Assign (Agent : in     Agent_Type;
                     Peer  :    out Peer_Type) is
   begin
      Peer.Agent_ID := Agent.ID;
   end Assign;

   --------------------
   --  Change_State  --
   --------------------

   procedure Change_State (Agent     :    out Agent_Type;
                           New_State : in     State) is
   begin
      Agent.Current_State := New_State;
   end Change_State;

   ---------------
   --  Context  --
   ---------------

   function Context (Agent : in Agent_Type) return String is
   begin
      return To_String (Agent.Context);
   end Context;

   --------------
   --  Create  --
   --------------

   function Create (ID        : in Agent_ID_Type;
                    Peer_ID   : in Peer_ID_Type;
                    Extension : in String) return Agent_Type is
   begin
      return (ID            => ID,
              Current_Call  => PBX.Call.Null_Identification,
              Current_State => Signed_Out,
              Name          => Null_Unbounded_String,
              Context       => Null_Unbounded_String,
              Peer_ID       => Peer_ID,
              Extension     => To_Unbounded_String (Extension));
   end Create;

   --------------------
   --  Current_Call  --
   --------------------

   function Current_Call (Agent : in Agent_Type)
                          return PBX.Call.Identification is
   begin
      return Agent.Current_Call;
   end Current_Call;

   --------------------
   --  Current_Call  --
   --------------------

   procedure Current_Call (Agent :    out Agent_Type;
                           Call  : in     PBX.Call.Identification) is
   begin
      Agent.Current_Call := Call;
   end Current_Call;
   ---------------------
   --  Current_State  --
   ---------------------

   function Current_State (Agent : in Agent_Type) return State is
   begin
      return Agent.Current_State;
   end Current_State;

   -----------------
   --  Extension  --
   -----------------

   function Extension (Agent : in Agent_Type) return String is
   begin
      return To_String (Agent.Extension);
   end Extension;

   -----------
   --  Get  --
   -----------

   function Get (Agent_ID : in Agent_ID_Type) return Agent_Type is
   begin
      return Agent_List.Element (Agent_ID);
   end Get;

   -----------
   --  Get  --
   -----------

   function Get (Peer_ID : Peer_ID_Type) return Agent_Type is
   begin
      if Peer_ID.Peername = "softphone1" then
         return Get (Agent_ID.Create ("1"));

      elsif Peer_ID.Peername = "softphone2" then
         return Get (Agent_ID.Create ("2"));

      elsif Peer_ID.Peername = "TP-Softphone" then
         return Get (Agent_ID.Create ("3"));

      elsif Peer_ID.Peername = "JSA-N900" then
         return Get (Agent_ID.Create ("4"));

      elsif Peer_ID.Peername = "DesireZ" then
         return Get (Agent_ID.Create ("5"));

      elsif Peer_ID.Peername = "TL-Softphone" then
         return Get (Agent_ID.Create ("6"));

      elsif Peer_ID.Peername = "uhh" then
         return Get (Agent_ID.Create ("7"));
      end if;

      return Null_Agent;
   end Get;

   ----------
   --  ID  --
   ----------

   function ID (Agent : in Agent_Type) return Agent_ID_Type is
   begin
      return Agent.ID;
   end ID;

   ---------------
   --  Peer_ID  --
   ---------------

   function Peer_ID (Agent : in Agent_Type) return Peer_ID_Type is
   begin
      return Agent.Peer_ID;
   end Peer_ID;

   function To_JSON (Agent : in Agent_Type) return GNATCOLL.JSON.JSON_Value is
      use GNATCOLL.JSON;

      JSON      : constant JSON_Value := Create_Object;
      Peer_JSON : constant JSON_Value := Create_Object;
   begin
      Peer_JSON.Set_Field ("ID", Agent.ID.To_String);
      Peer_JSON.Set_Field ("State", Agent.Current_State'Img);
      Peer_JSON.Set_Field ("Name", Agent.Name);
      Peer_JSON.Set_Field ("Context", Agent.Context);
      Peer_JSON.Set_Field ("Peer_ID", To_String (Agent.Peer_ID));
      Peer_JSON.Set_Field ("extension", Agent.Extension);
      Peer_JSON.Set_Field ("current_call",
                           PBX.Call.Get (Agent.Current_Call).To_JSON);
      JSON.Set_Field ("agent", Peer_JSON);

      return JSON;
   end To_JSON;

   function To_JSON return GNATCOLL.JSON.JSON_Value is
      use GNATCOLL.JSON;
         JSON_List : JSON_Array;
         Root      : constant JSON_Value := Create_Object;
   begin
      for Agent of Agent_List loop
         Append (JSON_List, Agent.To_JSON);
      end loop;
      Root.Set_Field ("agents", JSON_List);
      return Root;
   end To_JSON;

   procedure Update (Agent : in Agent_Type) is
   begin
      Agent_List.Replace (Agent.ID, Agent);
   end Update;
begin

   Agent_List.Insert
     (Agent_ID.Create (1),
      Agent.Create
        (ID        => Agent_ID.Create (1),
         Peer_ID   => AMI.Peer_ID.Create ("SIP/softphone1"),
         Extension => "101"));

   Agent_List.Insert
     (Agent_ID.Create (2),
      Agent.Create
        (ID        => Agent_ID.Create (2),
         Peer_ID   =>  AMI.Peer_ID.Create ("SIP/softphone1"),
         Extension => "102"));
   Agent_List.Insert
     (Agent_ID.Create (3),
      Agent.Create
        (ID        => Agent_ID.Create (3),
         Peer_ID   =>  AMI.Peer_ID.Create ("SIP/softphone1"),
         Extension => "103"));
   Agent_List.Insert
     (Agent_ID.Create (4),
      Agent.Create
        (ID        => Agent_ID.Create (4),
         Peer_ID   =>  AMI.Peer_ID.Create ("SIP/softphone1"),
         Extension => "104"));
   Agent_List.Insert
     (Agent_ID.Create (5),
      Agent.Create
        (ID        => Agent_ID.Create (5),
         Peer_ID   =>  AMI.Peer_ID.Create ("SIP/softphone1"),
         Extension => "105"));
   Agent_List.Insert
     (Agent_ID.Create (6),
      Agent.Create
        (ID        => Agent_ID.Create (6),
         Peer_ID   =>  AMI.Peer_ID.Create ("SIP/softphone1"),
         Extension => "106"));
   Agent_List.Insert
     (Agent_ID.Create (7),
      Agent.Create
        (ID        => Agent_ID.Create (7),
         Peer_ID   =>  AMI.Peer_ID.Create ("SIP/uhh"),
         Extension => "107"));

end Model.Agent;
