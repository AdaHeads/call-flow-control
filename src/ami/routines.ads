-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                 Routines                                  --
--                                                                           --
--                                  SPEC                                     --
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

--  ???? Perhaps rename to something a bit less generic?
--  PBX.Routines? PBX_Routines? PBX_Actions?
package Routines is
   use Ada.Strings.Unbounded;

   type Status_Type is
     (Success,
      No_Agent_Found,
      No_Call_Found,
      Unregistered_Agent,
      Agent_Already_In_Call,
      Unknown_Error);

   --------------------------------------------------------
   --  Should be out of the AMI directory.

   procedure Bridge_Call (Call_Id_1 : in     Unbounded_String;
                          Call_Id_2 : in     Unbounded_String;
                          Status    :    out Status_Type);

   procedure Get_Call (Unique_Id : in     String;
                       Agent_Id  : in     String;
--                         Call      :    out Call_List.Call_Type;
                       Status    :    out Status_Type);
   --  Takes a call from the call_Queue, and redirects it to the Agent.

--     procedure Get_Version; --  return String;

   procedure Park (Call_Id : in     String;
                   Status  :    out Status_Type);

   procedure UnPark ( --  Agent_ID : in     String;
                     Call_Id : in     String;
                     Status  :    out Status_Type);

   procedure Register_Agent (Phone_Name  : in Unbounded_String;
                             Computer_Id : in Unbounded_String);

   procedure Hangup (Call_Id : in     Unbounded_String;
                     Status  :    out Status_Type);

   --  Checks if the internal call queue is the same on Asterisk.
--     procedure Consistency_Check;

   procedure Startup_Sequence;
   --  TODO: Write comment.

   procedure Test_Status_Print;
   --  TODO: Write comment.
   ---------------------------------------------------------
end Routines;
