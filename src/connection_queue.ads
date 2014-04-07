with
  GNAT.Sockets;

package Connection_Queue is
   protected Object is
      entry Enqueue (Connection : in     GNAT.Sockets.Socket_Type);
      entry Dequeue (Connection :    out GNAT.Sockets.Socket_Type);
   private
      Has_Value : Boolean := False;
      Buffer    : GNAT.Sockets.Socket_Type;
   end Object;
end Connection_Queue;
