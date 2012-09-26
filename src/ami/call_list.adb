-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                Call_List                                  --
--                                                                           --
--                                  BODY                                     --
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

with Ada.Characters.Latin_1;
with Yolk.Log;

package body Call_List is

   protected Protected_Call_List is
      procedure Add (Call : in Call_Type);
      --  Places a call on the callqueue.

      --        procedure Dequeue (Call : out Call_Type);
      --        --  Takes the next call in the queue.
      --
      --        procedure Dequeue (Uniqueid : in     Unbounded_String;
      --                           Call     :    out Call_Type);
      --        --  Takes out a specific call from the callqueue.

      procedure Remove (Uniqueid : in      Unbounded_String;
                        Call     :    out  Call_Type);
      --  Removes a specific call.

      function Get return Call_List_Type.Vector;
      --  Returns the entire queue.

      function Get_Call (UniqueID : in Unbounded_String) return Call_Type;
      --  Returns the call with that UniqueID.

      function Length return Ada.Containers.Count_Type;
      --  Gives the number of calls waiting in the call queue.

      function ToString return Unbounded_String;
      --  Return a Debug friendly String representation of the list.

      --        procedure PickupCall (Agent_ID : in Unbounded_String,
      --                              Uniqueid : in Unbounded_String,
      --                              Call     : out Call_Type);

      procedure Update (Call : in Call_Type);
   private
      List : Call_List_Type.Vector;
   end Protected_Call_List;

   protected body Protected_Call_List is
   --   --  Gives the call with the highest priority, and have waited longest.
   --        procedure Dequeue (Call : out Call_Type) is
   --        begin
   --           Call := null_Call;
   --           for i in Queue'Range loop
   --              if not Queue_Type.Is_Empty (Queue (i)) then
   --                 Call := Queue_Type.First_Element (Queue (i));
   --                 Queue_Type.Delete_First (Container => Queue (i));
   --              end if;
   --           end loop;
   --        end Dequeue;

      --        --  Gives the call with selected Uniqueid
      --        procedure Dequeue (Uniqueid : in     Unbounded_String;
      --                           Call     :    out Call_Type) is
      --           Call_Queue_Cursor : Queue_Type.Cursor;
      --           Current_Call : Call_Type;
      --        begin
      --           for i in Queue'Range loop
      --      Call_Queue_Cursor := Queue_Type.First (Container => Queue (i));
      --              loop
      --                 exit when Call_Queue_Cursor = Queue_Type.No_Element;
      --               Current_Call := Queue_Type.Element (Call_Queue_Cursor);
      --                 if Current_Call.Uniqueid = Uniqueid then
      --                    Call := Current_Call;
      --                    Queue_Type.Delete (Container => Queue (i),
      --                                       Position  => Call_Queue_Cursor,
      --                                       Count     => 1);
      --                    return;
      --                 end if;
      --              end loop;
      --
      --              if not Queue_Type.Is_Empty (Queue (i)) then
      --                 Call := Queue_Type.First_Element (Queue (i));
      --                 Queue_Type.Delete_First (Container => Queue (i));
      --              end if;
      --           end loop;
      --           Call := null_Call;
      --        end Dequeue;

      --  Places the call, in the queue.
      procedure Add (Call : in Call_Type) is
      --           Company_Priority : constant Priority_Level :=
      --             Get_Company_Priority (Call.Queue);
      begin
         List.Append (Call);
         --           Queue_Type.Append (Container => Queue (Company_Priority),
         --                              New_Item  => Call);
      end Add;

      --  Returns the entire queue.
      function Get return Call_List_Type.Vector is
      begin
         return List;
      end Get;

      function Get_Call (UniqueID : in Unbounded_String) return Call_Type is
      begin
         for Call in List.Iterate loop
            if Call_List_Type.Element (Call).Uniqueid = UniqueID then
               return Call_List_Type.Element (Call);
            end if;
         end loop;
         return Null_Call;
      end Get_Call;

      --  Returns the total number of calls.
      function Length return Ada.Containers.Count_Type is
         use Ada.Containers;
      begin
         return List.Length;
      end Length;

      --  Removes the call with the specified UniqueID
      procedure Remove (Uniqueid : in     Unbounded_String;
                        Call     :    out Call_Type) is
      begin
         declare
            Cursor : Call_List_Type.Cursor;
         begin
            Cursor := List.First;
            loop
               exit when not Call_List_Type.Has_Element (Cursor);
               if Call_List_Type.Element (Cursor).Uniqueid = Uniqueid then
                  Call := Call_List_Type.Element (Cursor);
                  Call_List_Type.Delete (Container => List,
                                         Position  => Cursor,
                                         Count     => 1);
                  return;
               end if;
            end loop;
         end;

         Yolk.Log.Trace
           (Yolk.Log.Debug,
            "Call_List - Remove:" &
              "This uniqueid could not be found in the call queue." &
              " Uniqueid: " & To_String (Uniqueid));
         Call := Null_Call;
      end Remove;

      --  Returns the call list as String.
      function ToString return Unbounded_String is
         Text : Unbounded_String;
         package Char renames Ada.Characters.Latin_1;
      begin
         for Call in List.Iterate loop
            Append (Text, Call_List_Type.Element (Call).Channel);
            Append (Text, ", Uniqueid: ");
            Append (Text, Call_List_Type.Element (Call).Uniqueid);
            Append (Text, Char.LF);
         end loop;
         return Text;
      end ToString;

      --        procedure PickupCall (Agent_ID : in Unbounded_String,
      --                              Uniqueid : in Unbounded_String
      --                              Call     : out Call_Type) is
      --           Temp_Call : Call_Type;
      --        begin
      --           --  First find a call
      --           if Uniqueid = Null_Unbounded_String then
      --              for item in List loop
      --                 if item.State = Queued then
      --                    Temp_Call := item;
      --                 end if;
      --              end loop;
      --           else
      --              for item in List loop
      --                 if item.Uniqueid = Uniqueid then
      --                    if item.State /= Queued then
      --                       Yolk.Log.Trace
      --                         (Yolk.Log.Debug,
      --                          "Call_List - PickupCall:" &
      --                  "The Call with Uniqueid: " & To_String (Uniqueid) &
      --                         " is in state: " item.State'Img);
      --                    else
      --                       Temp_Call := item;
      --                    end if;
      --                 end if;
      --              end loop;
      --           end if;
      --
      --           --  Second insert Agent_ID and change some fields.
      --           Call.Agent_ID = Agent_ID;
      --           Call.State = Speaking;
      --           Call.Is_Picked_Up = True;
      --           Call.Picked_Up = Ada.Calendar.Time;
      --
      --           --  Thrid update Call element.
      --           Update (Temp_Call);
      --
      --           --  fourth return the call
      --           Call := Temp_Call;
      --        end PickupCall;

      procedure Update (Call : in Call_Type) is
      begin
         for Index in Integer range List.First_Index .. List.Last_Index loop
            if List.Element (Index).Uniqueid = Call.Uniqueid then
               List.Replace_Element (Index => Index, New_Item => Call);
               return;
            end if;
         end loop;
      end Update;
   end Protected_Call_List;

   --  Places a call on the call queue.
   procedure Add (Call : in Call_Type) is
   begin
      Protected_Call_List.Add (Call);
   end Add;

   --  Returns a call in String format.
   function Call_To_String (Call : in Call_Type) return String is
      Response : Unbounded_String;
   begin
      Append (Response, "Channel => " & To_String (Call.Channel));
      Append (Response, ", Queue => " & To_String (Call.Queue));
      Append (Response, ", Uniqueid => " & To_String (Call.Uniqueid));
      return To_String (Response);
   end Call_To_String;

   --     --  Takes the first call with the highest priority
   --     procedure Dequeue (Call : out Call_Type) is
   --     begin
   --        Call_Queue.Dequeue (Call);
   --     end Dequeue;

   --     --  Takes a specific call out from the call queue.
   --  procedure Dequeue
--       (Uniqueid : in Unbounded_String; Call : out Call_Type) is
   --     begin
   --        Call_Queue.Dequeue (Uniqueid, Call);
   --     end Dequeue;

   --     --  Returns the priority of the call, based on the company
   --     function Get_Company_Priority (CompanyName : Unbounded_String)
   --                                    return Priority_Level is
   --     begin
   --        if To_String (CompanyName) = "org_id1" then
   --           return High;
   --        elsif To_String (CompanyName) = "org_id2" then
   --           return Normal;
   --        else
   --           return Low;
   --        end if;
   --     end Get_Company_Priority;

   --  Returns the entire call queue.
   function Get return Call_List_Type.Vector is
   begin
      return Protected_Call_List.Get;
   end Get;

   function Get_Call (UniqueID : in Unbounded_String) return Call_Type is
   begin
      return Protected_Call_List.Get_Call (UniqueID);
   end Get_Call;

   --  Gives the length of the call queue.
   function Length return Ada.Containers.Count_Type is
   begin
      return Protected_Call_List.Length;
   end Length;

   --  Removes a specific call from the call queue.
   --  if the call is not in the list, a null_call is returned otherwise,
   --   the call is returned.
   function Remove (Uniqueid : in Unbounded_String) return Call_Type is
      Call : Call_Type;
   begin
      Protected_Call_List.Remove (Uniqueid, Call);
      return Call;
   end Remove;

   --  Returns a debug friendly String representation of the call queue.
   function ToString return Unbounded_String is
   begin
      return Protected_Call_List.ToString;
   end ToString;

   --     function PickupCall (Agent_ID : in Unbounded_String,
   --         Uniqueid : in Unbounded_String) return Call_Type is
   --        Call : Call_Type;
   --     begin
   --        Protected_Call_List.PickupCall (Agent_ID, Uniqueid, Call);
   --        return Call;
   --     end PickupCall;

   procedure Update (Call : in Call_Type) is
   begin
      Protected_Call_List.Update (Call);
   end Update;

   --     function Hangup
   --       (CallID : in Unbounded_String) return Call_Type is
   --        Call : Call_Type;
   --     begin
   --        Protected_Call_List.Hangup (CallID, Call);
   --        return Call;
   --     end Hangup;

end Call_List;
