with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Call_Queue;
package Routines is
   --------------------------------------------------------
   --  Should be out of the AMI directory.

   procedure Bridge_Call (Channel1 : in Unbounded_String;
                          Channel2 : in Unbounded_String);

   procedure Get_Call (Uniqueid   : in     String;
                       Agent      : in     String;
                       Call       :    out Call_Queue.Call_Type);
   --  Takes a call from the call_Queue, and redirects it to the Agent.

   function Get_Version return String;

   procedure Park (Agent : in Unbounded_String);

   procedure Register_Agent (PhoneName   : in Unbounded_String;
                             Computer_ID : in Unbounded_String);

   procedure Consistency_Check;

   procedure StartUpSequence;
   procedure TEST_StatusPrint;
   ---------------------------------------------------------
end Routines;
