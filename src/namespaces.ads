with Black.Request;

package Namespaces is
   type Resources is (Not_Found, Call, User);

   function Resource (Item : in Black.Request.Instance) return Resources;
end Namespaces;
