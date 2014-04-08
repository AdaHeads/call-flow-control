with Ada.Strings.Fixed;

package body Namespaces is
   function Name (Item : in String;
                  Head : in String := "/") return String;

   function Call_Resource (Item : in Black.Request.Instance)
                          return Call_Resources is
      URI : constant String := Item.Resource;
   begin
      return Call_Resources'Value (Name (Item => URI,
                                         Head => "/call/"));
   exception
      when others =>
         return Not_Found;
   end Call_Resource;

   function Name (Item : in String;
                  Head : in String := "/") return String is
      use Ada.Strings;
      From      : Positive;
      Separator : Natural := Item'First;
   begin
      if Head = Fixed.Head (Item, Head'Length) then
         From := Item'First + Head'Length;

         Separator := Fixed.Index (Source  => Item (From .. Item'Last),
                                   Pattern => "/");

         if Separator = 0 then
            Separator := Fixed.Index (Source  => Item (From .. Item'Last),
                                      Pattern => "?");
         end if;

         if Separator = 0 then
            Separator := Item'Last + 1;
         end if;

         return Item (From .. Separator - 1);
      else
         return "";
      end if;
   end Name;

   function Resource (Item : in Black.Request.Instance) return Resources is
      URI : constant String := Item.Resource;
   begin
      return Resources'Value (Name (Item => URI,
                                    Head => "/"));
   exception
      when others =>
         return Not_Found;
   end Resource;

   function Users_Resource (Item : in Black.Request.Instance)
                           return Users_Resources is
      URI : constant String := Item.Resource;
   begin
      return Users_Resources'Value (Name (Item => URI,
                                          Head => "/users/"));
   exception
      when others =>
         return Not_Found;
   end Users_Resource;
end Namespaces;
