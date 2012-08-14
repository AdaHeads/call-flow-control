with Ada.Strings.Unbounded;
with Call_Queue;
package Routines is
   use Ada.Strings.Unbounded;

   type Status_Type is
     (Success,
      No_Agent_Found,
      No_Call_Found,
      Unregistred_Agent,
      Agent_Already_In_Call,
      Unknowen_Error);

   --------------------------------------------------------
   --  Should be out of the AMI directory.

   procedure Bridge_Call (Channel1 : in Unbounded_String;
                          Channel2 : in Unbounded_String);

   procedure Get_Call (Uniqueid : in     String;
                       Agent    : in     String;
                       Call     :    out Call_Queue.Call_Type;
                       Status   :    out Status_Type);
   --  Takes a call from the call_Queue, and redirects it to the Agent.

   function Get_Version return String;

   procedure Park (Agent  : in     String;
                   Call   :    out Call_Queue.Call_Type;
                   Status :    out Status_Type);

   procedure UnPark (Agent    : in     String;
                     Call_ID  : in     String;
                     Status   :    out Status_Type);

   procedure Register_Agent (PhoneName   : in Unbounded_String;
                             Computer_ID : in Unbounded_String);

   procedure Hangup (Agent  : in     Unbounded_String;
                     Status :    out Status_Type);

   --  Checks if the internal call queue is the same on Asterisk.
   procedure Consistency_Check;

   procedure StartUpSequence;
   procedure TEST_StatusPrint;
   ---------------------------------------------------------
end Routines;
