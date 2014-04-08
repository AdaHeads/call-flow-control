with Black.Request;

package Namespaces is
   type Resources       is (Not_Found, Call, Users);
   type Call_Resources  is (Not_Found, Hangup, List, Originate, Park, Pickup,
                            Queue, Transfer);
   type Users_Resources is (Not_Found, List, Pause);

   function Resource (Item : in Black.Request.Instance)
                     return Resources;
   function Call_Resource (Item : in Black.Request.Instance)
                          return Call_Resources;
   function Users_Resource (Item : in Black.Request.Instance)
                           return Users_Resources;
end Namespaces;
