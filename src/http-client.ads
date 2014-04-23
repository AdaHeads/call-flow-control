with Black.MIME_Types,
     Black.Response;

package HTTP.Client is
   function Get (URL : in String) return Black.Response.Class;

   function Post (URL          : in String;
                  Data         : in String;
                  Content_Type : in String := Black.MIME_Types.Text.Plain)
                 return Black.Response.Class;
end HTTP.Client;
