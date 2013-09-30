-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                     --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with AWS.Parameters;
with AWS.Response.Set;
with AWS.Session;
with AWS.URL;
with HTTP_Codes;
with Model;
with System_Message.Error;

package body Response is

   Missing_Required_Request_Parameter : exception;

   JSON_MIME_Type : constant String := "application/json; charset=utf-8";

   procedure Add_CORS_Headers
     (Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data);
   --  If the client sends the Origin: header, add these two CORS headers:
   --    Access-Control-Allow-Origin
   --    Access-Control-Allow-Credentials
   --  where the first one should contain the value of the given
   --  Origin : header and the second a Boolean True. This should be enough
   --  to enable very simple CORS support in Alice.

   function Add_JSONP_Callback
     (Content : in Common.JSON_String;
      Request : in AWS.Status.Data)
      return Common.JSON_String;
   --  Wrap Content in jsoncallback(Content) if the jsoncallback parameter
   --  is given in the Request. jsonpcallback is replaced with the actual
   --  value of the jsoncallback parameter.
   --  NOTE:
   --  We do not support the callback parameter. It is too generic.

   --  Validate procedures
   --  These are used to cast a string into one of the Value types. If the
   --  cast fails with a constraint error, then we know the given value is
   --  invalid.

   procedure Validate
     (Value : in Model.Contact_Identifier)
   is null;

   procedure Validate
     (Value : in Model.Organization_Identifier)
   is null;

   procedure Validate
     (Value : in Request_Parameters.List_View)
   is null;

   ------------------------
   --  Add_CORS_Headers  --
   ------------------------

   procedure Add_CORS_Headers
     (Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data)
   is
      use AWS.Messages;
      use AWS.Response;
      use AWS.Status;

      Origin_Host : constant String := Origin (Request);
   begin
      if Origin_Host'Length > 0 then
         Set.Add_Header (D     => Response,
                         Name  => Access_Control_Allow_Origin_Token,
                         Value => Origin_Host);

         Set.Add_Header (D     => Response,
                         Name  => Access_Control_Allow_Credentials_Token,
                         Value => "true");

         Set.Add_Header (D     => Response,
                         Name  => Access_Control_Allow_Headers_Token,
                         Value => "POST, GET");

         Set.Add_Header (D     => Response,
                         Name  => Access_Control_Max_Age_Token,
                         Value => "86400");
      end if;
   end Add_CORS_Headers;

   --------------------------
   --  Add_JSONP_Callback  --
   --------------------------

   function Add_JSONP_Callback
     (Content : in Common.JSON_String;
      Request : in AWS.Status.Data)
      return Common.JSON_String
   is
      use Ada.Strings;
      use AWS.Status;
      use Common;

      JSON_Callback : constant String := Fixed.Trim
        (Parameters (Request).Get ("jsoncallback"), Both);
   begin
      if JSON_Callback'Length > 0 then
         return To_Unbounded_String (JSON_Callback)
           & To_Unbounded_String ("(")
           & Content
           & To_Unbounded_String (")");
      end if;

      return Content;
   end Add_JSONP_Callback;

   -------------
   --  Build  --
   -------------

   function Build
     (Instance : in Object)
      return AWS.Response.Data
   is
      use AWS.Messages;
      use AWS.Response;
      use AWS.Status;
      use Common;

      D        : AWS.Response.Data;
      Encoding : constant Content_Encoding :=
                   Preferred_Coding (Instance.Status_Data);
   begin
      D :=  Build (Content_Type  => JSON_MIME_Type,
                   Message_Body  =>
                     To_String (Add_JSONP_Callback
                       (Instance.Content, Instance.Status_Data)),
                   Status_Code   => Instance.HTTP_Status_Code,
                   Encoding      => Encoding,
                   Cache_Control => No_Cache);

      Add_CORS_Headers (Instance.Status_Data, D);

      return D;
   end Build;

   ---------------
   --  Content  --
   ---------------

   procedure Content
     (Instance : in out Object;
      Value    : in     Common.JSON_String)
   is
   begin
      Instance.Content := Value;
   end Content;

   ---------------
   --  Factory  --
   ---------------

   function Factory
     (Request : in AWS.Status.Data)
      return Object
   is
   begin
      return Instance : Object do
         Instance.Content := Common.Null_JSON_String;
         Instance.HTTP_Status_Code := HTTP_Codes.OK;
         Instance.Is_Cacheable := False;
         Instance.Status_Data := Request;
         Instance.Status_Data_Set := True;
         Instance.Valid_Request_Parameters := False;
         Instance.Validations := Set_List.Empty_List;
      end return;
   end Factory;

   ---------------
   --  Factory  --
   ---------------

   function Factory
     return Object
   is
   begin
      return Instance : Object do
         Instance.Content := Common.Null_JSON_String;
         Instance.HTTP_Status_Code := HTTP_Codes.OK;
         Instance.Is_Cacheable := False;
         Instance.Status_Data_Set := False;
         Instance.Valid_Request_Parameters := False;
         Instance.Validations := Set_List.Empty_List;
      end return;
   end Factory;

   -----------------------
   --  Has_Status_Data  --
   -----------------------

   function Has_Status_Data
     (Instance : in Object)
      return Boolean
   is
   begin
      return Instance.Status_Data_Set;
   end Has_Status_Data;

   ------------------------
   --  HTTP_Status_Code  --
   ------------------------

   procedure HTTP_Status_Code
     (Instance : in out Object;
      Value    : in     AWS.Messages.Status_Code)
   is
   begin
      Instance.HTTP_Status_Code := Value;
   end HTTP_Status_Code;

   --------------------
   --  Is_Cacheable  --
   --------------------

   function Is_Cacheable
     (Instance : in Object)
      return Boolean
   is
   begin
      return Instance.Is_Cacheable;
   end Is_Cacheable;

   --------------------
   --  Is_Cacheable  --
   --------------------

   procedure Is_Cacheable
     (Instance : in out Object;
      Value    : in     Boolean)
   is
   begin
      Instance.Is_Cacheable := Value;
   end Is_Cacheable;

   -----------------
   --  Parameter  --
   -----------------

   function Parameter
     (Instance : in Object;
      Name     : in String)
      return String
   is
      use AWS.Status;
   begin
      return Parameters (Instance.Status_Data).Get (Name);
   end Parameter;

   -----------------------
   --  Parameter_Count  --
   -----------------------

   function Parameter_Count
     (Instance : in Object)
      return Natural
   is
      use AWS.Status;
   begin
      return Parameters (Instance.Status_Data).Count;
   end Parameter_Count;

   -----------------------
   --  Parameter_Exist  --
   -----------------------

   function Parameter_Exist
     (Instance : in Object;
      Name     : in String)
      return Boolean
   is
      use AWS.Status;
   begin
      return Parameters (Instance.Status_Data).Exist (Name);
   end Parameter_Exist;

   ----------------------------------
   --  Register_Request_Parameter  --
   ----------------------------------

   procedure Register_Request_Parameter
     (Instance       : in out Object;
      Mode           : in     Request_Parameters.Mode;
      Parameter_Name : in     String;
      Validate_As    : in     Request_Parameters.Kind)
   is
      use Common;
   begin
      Instance.Validations.Append ((Mode, U (Parameter_Name), Validate_As));
      Instance.Valid_Request_Parameters := False;
   end Register_Request_Parameter;

   -------------------
   --  Request_URL  --
   -------------------

   function Request_URL
     (Instance : in Object)
      return String
   is
      use AWS.Status;
      use AWS.URL;
   begin
      return URL (URI (Instance.Status_Data));
   end Request_URL;

   -------------------
   --  Status_Data  --
   -------------------

   function Status_Data
     (Instance : in Object)
      return AWS.Status.Data
   is
   begin
      return Instance.Status_Data;
   end Status_Data;

   -------------------
   --  Status_Data  --
   -------------------

   procedure Status_Data
     (Instance : in out Object;
      Request  : in     AWS.Status.Data)
   is
   begin
      Instance.Status_Data := Request;
      Instance.Status_Data_Set := True;
   end Status_Data;

   -----------------------
   --  To_Debug_String  --
   -----------------------

   function To_Debug_String
     (Instance : in Object)
      return String
   is
      use AWS.Session;
      use AWS.Status;

      Session_String : Unbounded_String;

      procedure Action
        (N          : in     Positive;
         Key, Value : in     String;
         Kind       : in     AWS.Session.Value_Kind := AWS.Session.Str;
         Quit       : in out Boolean);
      --  Create a human-readable string from users session data.

      --------------
      --  Action  --
      --------------
      procedure Action
        (N          : in     Positive;
         Key, Value : in     String;
         Kind       : in     AWS.Session.Value_Kind := AWS.Session.Str;
         Quit       : in out Boolean)
      is
         pragma Unreferenced (N, Quit);
      begin
         Append (Session_String, Key & ":" & Value & " ");
      end Action;

      procedure Session_Data_Reader is new AWS.Session.For_Every_Session_Data
        (Action => Action);

      Msg : Unbounded_String;
   begin
      Append (Msg, Instance.Request_URL & " - ");
      Append (Msg, "Peer " & Peername (Instance.Status_Data) & " - ");
      Append (Msg, "User-Agent " & User_Agent (Instance.Status_Data));

      if Has_Session (Instance.Status_Data) then
         Append (Msg, " - ");
         Session_Data_Reader (Session (Instance.Status_Data));
         Append (Msg, "Session ID " & Image (Session (Instance.Status_Data)));

         if Length (Session_String) > 0 then
            Append (Msg,  " - " & "Session data " & Session_String);
         else
            Append (Msg, " - No Session data");
         end if;
      end if;

      return To_String (Msg);
   end To_Debug_String;

   --------------------------------
   --  Valid_Request_Parameters  --
   --------------------------------

   function Valid_Request_Parameters
     (Instance : in Object)
      return Boolean
   is
   begin
      return Instance.Valid_Request_Parameters;
   end Valid_Request_Parameters;

   -----------------------------------
   --  Validate_Request_Parameters  --
   -----------------------------------

   procedure Validate_Request_Parameters
     (Instance : in out Object)
   is
      use Ada.Strings.Fixed;
      use AWS.Parameters;
      use AWS.Status;
      use Common;
      use System_Message;
      use type Request_Parameters.Mode;

      Param_List : constant List := Parameters (Instance.Status_Data);
      Set        : Validation_Set;
   begin
      for Elem of Instance.Validations loop
         Set := Elem;

         if Param_List.Exist (To_String (Set.Name)) then
            case Elem.Validate_As is
               when Request_Parameters.Contact_Identifier =>
                  Validate
                    (Model.Contact_Identifier'Value
                       (Param_List.Get (To_String (Elem.Name))));
               when Request_Parameters.Non_Empty_String =>
                  if Trim (Param_List.Get (To_String (Elem.Name)),
                           Ada.Strings.Both)'Length < 1
                  then
                     raise Constraint_Error;
                  end if;
               when Request_Parameters.Organization_Identifier =>
                  Validate
                    (Model.Organization_Identifier'Value
                       (Param_List.Get (To_String (Elem.Name))));
               when Request_Parameters.Organization_List_View =>
                  Validate
                    (Request_Parameters.List_View'
                       Value (Param_List.Get (To_String (Elem.Name))));
            end case;
         else
            if Set.Mode = Request_Parameters.Required then
               raise Missing_Required_Request_Parameter;
            end if;
         end if;
      end loop;

      Instance.Valid_Request_Parameters := True;
      --  If we reach this point, the request parameters are valid, even if
      --  there aren't any at all.
   exception
      when Constraint_Error =>
         Error.Bad_Request_Parameter
           (Message         => To_String (Set.Name)
            & " must be a valid "
            & Request_Parameters.Kind'Image (Set.Validate_As),
            Response_Object => Instance);
      when Missing_Required_Request_Parameter =>
         Error.Missing_Request_Parameter
           (Message         => To_String (Set.Name)
            & " not found ",
            Response_Object => Instance);
   end Validate_Request_Parameters;

end Response;
