with
  Ada.Characters.Handling,
  Ada.Strings.Fixed;
with
  GNAT.Sockets;
with
  Black.HTTP,
  Black.Request,
  GNAT.Sockets.Convenience;

package body HTTP.Client is
   function Host     (URL : in String) return String;
   function Port     (URL : in String) return GNAT.Sockets.Port_Type;
   function Resource (URL : in String) return String;

   function Get (URL : in String) return Black.Response.Class is
      use GNAT.Sockets;
      Connection : Socket_Type;
   begin
      Connection := Convenience.Connect_To_Server (Host => Host (URL),
                                                   Port => Port (URL));

      Black.Request.Instance'Output
        (Stream (Connection),
         Black.Request.Compose (Method   => Black.HTTP.Get,
                                Host     => Host (URL),
                                Resource => Resource (URL)));

      return Black.Response.Instance'Input (Stream (Connection));
   end Get;

   function Host (URL : in String) return String is
      use Ada.Characters.Handling, Ada.Strings.Fixed;
      Prefix : constant String := "http://";
      Last   : Positive;
   begin
      if To_Lower (Head (URL, Prefix'Length)) = Prefix then
         Last := URL'First + Prefix'Length;
         while Last <= URL'Last loop
            exit when not (Is_Alphanumeric (URL (Last)) or
                           URL (Last) = '-' or
                           URL (Last) = '.');
            Last := Last + 1;
         end loop;

         return URL (URL'First + Prefix'Length .. Last);
      else
         raise Constraint_Error
           with "We only do plain HTTP connections.";
      end if;
   end Host;

   function Port (URL : in String) return GNAT.Sockets.Port_Type is
      use Ada.Characters.Handling, Ada.Strings.Fixed;
      Prefix : constant String := "http://" & Host (URL);
      First  : Positive;
      Last   : Positive;
   begin
      if To_Lower (Head (URL, Prefix'Length)) = Prefix then
         First := URL'First + Prefix'Length;

         if First > URL'Last then
            --  Default port
            return 80;
         elsif URL (First) = ':' then
            --  Port number included
            Last := First + 1;
            loop
               exit when Last > URL'Last;
               exit when URL (Last) not in '0' .. '9';
               Last := Last + 1;
            end loop;

            return GNAT.Sockets.Port_Type'Value (URL (First + 1 .. Last - 1));
         else
            --  Default port
            return 80;
         end if;
      else
         raise Constraint_Error
           with "We only do plain HTTP connections.";
      end if;
   end Port;

   function Post (URL          : in String;
                  Data         : in String;
                  Content_Type : in String := Black.MIME_Types.Text.Plain)
                 return Black.Response.Class is
      use GNAT.Sockets;
      Connection : Socket_Type;
   begin
      Connection := Convenience.Connect_To_Server (Host => Host (URL),
                                                   Port => Port (URL));

      Black.Request.Instance'Output
        (Stream (Connection),
         Black.Request.Compose (Method       => Black.HTTP.Post,
                                Host         => Host (URL),
                                Resource     => Resource (URL),
                                Content      => Data,
                                Content_Type => Content_Type));

      return Black.Response.Instance'Input (Stream (Connection));
   end Post;

   function Resource (URL : in String) return String is
      use Ada.Characters.Handling, Ada.Strings.Fixed;
      Prefix : constant String := "http://" & Host (URL);
      First  : Positive;
   begin
      if To_Lower (Head (URL, Prefix'Length)) = Prefix then
         First := URL'First + Prefix'Length;

         if First in URL'Range and then URL (First) = ':' then
            --  Port number included - skip it
            loop
               First := First + 1;
               exit when First > URL'Last;
               exit when URL (First) not in '0' .. '9';
            end loop;
         end if;

         if First > URL'Last then
            return "/";
         else
            return URL (First .. URL'Last);
         end if;
      else
         raise Constraint_Error
           with "We only do plain HTTP connections.";
      end if;
   end Resource;
end HTTP.Client;
