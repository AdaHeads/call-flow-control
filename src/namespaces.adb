package Namespaces is
   function Name (Item  : in String;
                  Index : in Positive) return String;

   function Name (Item  : in String;
                  Index : in Positive) return String is
      From      : Positive;
      Separator : Natural := Item'First;
   begin
      for Counter in 1 .. Index loop
         First := Separator;

         if Item (First) = '/' then
            Separator := Index (Source  => Item (First + 1 .. Item'Last),
                                Pattern => "/");

            if Separator = 0 then
               Separator := Index (Source  => Item (First + 1 .. Item'Last),
                                   Pattern => "?");
            end if;

            if Separator = 0 then
               Separator = Index'Last + 1;
            end if;
         else
            raise Black.Protocol_Error
              with "Resource should start with '/'.";
         end if;
      end loop;

      return Item (From + 1 .. Separator - 1);
   end Name;

   function Resource (Item : in Black.Request.Instance) return Resources is
      URI : constant String := Item.Resource;
   begin
      return Resources'Value (Name (URI, 1));
   exception
      when others =>
         return Not_Found;
   end Resource;
end Namespaces;
