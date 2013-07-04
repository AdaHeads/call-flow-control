--  Package for arbitrary constants. Just stuff'em here, and hope for the
--  best in a future refactoring.

package PBX.Magic_Constants is
   FIFO             : constant String := "fifo";
   Separator        : constant String := "::";
   FIFO_Info        : constant String := FIFO & Separator & "info";
   FIFO_Push        : constant String := "push";
   FIFO_Abort       : constant String := "abort";
   FIFO_Pop         : constant String := "caller_pop";
   Sofia            : constant String := "sofia";
   Sofia_Register   : constant String := Sofia & Separator & "register";
   Sofia_Unregister : constant String := Sofia & Separator & "register";

end PBX.Magic_Constants;
