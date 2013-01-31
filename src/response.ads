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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
with AWS.Messages;
with AWS.Response;
with AWS.Status;
with Common;
with Request_Parameters;

package Response is

   type Object is tagged limited private;

   function Build
     (Instance : in Object)
      return AWS.Response.Data
   with Pre => Instance.Has_Status_Data;
   --  Build the response and compress it if the client supports it. Also
   --  wraps JSON string in foo(JSON string) if the
   --      ?jsoncallback=foo
   --  GET parameter is present.

   procedure Content
     (Instance :    out Object;
      Value    : in     Common.JSON_String);
   --  Add content to Instance.

   function Factory
     return Object;
   --  At this point the response object is unaware of the request data,
   --  meaning it knows nothing about GET/POST parameters, sessions or anything
   --  else pertaining to the request made by the client. To make Object aware
   --  of the client request data you should call the Status_Data procedure
   --  at some point.

   function Factory
     (Request : in AWS.Status.Data)
      return Object;
   --  Wraps a Factory and Status_Data call into one convenient package.

   procedure HTTP_Status_Code
     (Instance :    out Object;
      Value    : in     AWS.Messages.Status_Code);
   --  Set the HTTP code that is returned to the client when Build is called.

   function Has_Status_Data
     (Instance : in Object)
      return Boolean;
   --  Return whether or not the AWS.Status.Data request object has been added
   --  to Instance.

   function Is_Cacheable
     (Instance : in Object)
      return Boolean;
   --  Return whether or not the content of Instance can be cached.

   procedure Is_Cacheable
     (Instance :    out Object;
      Value    : in     Boolean)
   with Post => Value = Instance.Is_Cacheable;
   --  Set whether the contents of Instance is cacheable. This does not define
   --  whether or not the content of Instance is cached - it merely states that
   --  it _can_ be cached.

   function Parameter
     (Instance : in Object;
      Name     : in String)
      return String;
   --  Return the Name request parameter. Return empty string if Name does not
   --  exist. Note that this does _NOT_ take validation into account. Calling
   --  this prior to Validate_Request_Parameters will return whatever value
   --  that might be associated with the Name key or empty String if Name does
   --  not exist.

   function Parameter_Count
     (Instance : in Object)
      return Natural
   with Pre => Instance.Has_Status_Data;
   --  Return the amount of request parameters in the status data request
   --  object.

   function Parameter_Exist
     (Instance : in Object;
      Name     : in String)
      return Boolean
   with Pre => Instance.Has_Status_Data;
   --  Return True if the Name request parameter exists.

   procedure Register_Request_Parameter
     (Instance       : in out Object;
      Mode           : in     Request_Parameters.Mode;
      Parameter_Name : in     String;
      Validate_As    : in     Request_Parameters.Kind)
   with Post => not Instance.Valid_Request_Parameters;
   --  Register a request parameter for later validation.

   function Request_URL
     (Instance : in Object)
      return String
   with Pre => Instance.Has_Status_Data;
   --  Return the full request URL string for Instance. This is basically just
   --  a wrapper for AWS.URL.URL (AWS.Status.URI (Instance.Status_Data))

   function Status_Data
     (Instance : in Object)
      return AWS.Status.Data
   with Pre => Instance.Has_Status_Data;
   --  Return the AWS.Status.Data object from Instance.

   procedure Status_Data
     (Instance : in out Object;
      Request  : in     AWS.Status.Data)
   with Post => Instance.Has_Status_Data;
   --  Set the client request data. This makes the response object aware of
   --  Cookies, Sessions, GET/POST request parameters and everything else that
   --  the AWS.Status.Data object contains.

   function To_Debug_String
     (Instance : in Object)
      return String;
   --  Generate a debug message containing as much information as we can force
   --  from the grubby hands of the original AWS.Status.Data object.

   function Valid_Request_Parameters
     (Instance : in Object)
      return Boolean;
   --  Return whether or not the registered request parameters are valid. Will
   --  ALWAYS return False if called prior to Validate_Request_Parameters.

   procedure Validate_Request_Parameters
     (Instance : in out Object)
   with Pre => Instance.Has_Status_Data;
   --  Validate the registered request parameters. If the validation fails,
   --  the Instance response object is updated to reflect the problem and
   --  Valid_Request_Parameters will remain false.

private

   use Ada.Containers;
   use Ada.Strings.Unbounded;

   type Validation_Set is
      record
         Mode        : Request_Parameters.Mode;
         Name        : Unbounded_String;
         Validate_As : Request_Parameters.Kind;
      end record;

   package Set_List is new Doubly_Linked_Lists
     (Element_Type => Validation_Set);

   type Object is tagged limited
      record
         Content                  : Common.JSON_String;
         HTTP_Status_Code         : AWS.Messages.Status_Code;
         Is_Cacheable             : Boolean;
         Status_Data              : AWS.Status.Data;
         Status_Data_Set          : Boolean;
         Valid_Request_Parameters : Boolean;
         Validations              : Set_List.List;
      end record;

end Response;
