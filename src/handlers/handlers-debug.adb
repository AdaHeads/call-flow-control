with Common;
with HTTP_Codes;
with Response;
with Model.Peers;

package body Handlers.Debug is
   use Common;
   use Model;

   function Channel_List (Request : in AWS.Status.Data)
                          return AWS.Response.Data is
      use HTTP_Codes;

      Response_Object : Response.Object := Response.Factory (Request);
   begin
      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content
        (To_JSON_String (Peers.List.To_JSON.Write));

      return Response_Object.Build;
   end Channel_List;

   function Peer_List (Request : in AWS.Status.Data)
                       return AWS.Response.Data is
      use HTTP_Codes;

      Response_Object : Response.Object := Response.Factory (Request);
   begin
      Response_Object.HTTP_Status_Code (OK);
      Response_Object.Content
        (To_JSON_String (Peers.List.To_JSON.Write));

      return Response_Object.Build;
   end Peer_List;

end Handlers.Debug;
