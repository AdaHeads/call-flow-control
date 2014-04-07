package body Connection_Queue is
   protected body Object is
      entry Enqueue (Connection : in     GNAT.Sockets.Socket_Type)
        when not Has_Value is
      begin
         Has_Value := True;
         Buffer    := Connection;
      end Enqueue;

      entry Dequeue (Connection :    out GNAT.Sockets.Socket_Type)
        when Has_Value is
      begin
         Has_Value  := False;
         Connection := Buffer;
      end Dequeue;
   end Object;
end Connection_Queue;
