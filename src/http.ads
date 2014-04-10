with Black.Request,
     Black.Response;

package HTTP is
   type Callback is access function (Request : in Black.Request.Instance)
                             return Black.Response.Class;
end HTTP;
