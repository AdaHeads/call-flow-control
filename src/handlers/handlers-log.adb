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

with Request_Parameters;
with System_Message.Critical;
with System_Message.Error;
with System_Message.Info;

package body Handlers.Log is

   function Get_Message
     (Instance : in out Response.Object)
      return String;
   --  TODO: Write comment.

   -------------------------
   --  Callback_Critical  --
   -------------------------

   function Callback_Critical
     return AWS.Dispatchers.Callback.Handler
   is
   begin
      return AWS.Dispatchers.Callback.Create (Critical_Response'Access);
   end Callback_Critical;

   ----------------------
   --  Callback_Error  --
   ----------------------

   function Callback_Error
     return AWS.Dispatchers.Callback.Handler
   is
   begin
      return AWS.Dispatchers.Callback.Create (Error_Response'Access);
   end Callback_Error;

   ---------------------
   --  Callback_Info  --
   ---------------------

   function Callback_Info
     return AWS.Dispatchers.Callback.Handler
   is
   begin
      return AWS.Dispatchers.Callback.Create (Info_Response'Access);
   end Callback_Info;

   --------------------
   --  Critical_Log  --
   --------------------

   procedure Critical_Log
     (Instance : in out Response.Object)
   is
      use System_Message;

      Msg : constant String := Get_Message (Instance);
   begin
      if Msg'Length > 0 then
         Critical.Client_Critical (Message         => Msg,
                                   Response_Object => Instance);
      end if;
   end Critical_Log;

   -----------------
   --  Error_Log  --
   -----------------

   procedure Error_Log
     (Instance : in out Response.Object)
   is
      use System_Message;

      Msg : constant String := Get_Message (Instance);
   begin
      if Msg'Length > 0 then
         Error.Client_Error (Message         => Msg,
                             Response_Object => Instance);
      end if;
   end Error_Log;

   -------------------
   --  Get_Message  --
   -------------------

   function Get_Message
     (Instance : in out Response.Object)
      return String
   is
   begin
      Instance.Register_Request_Parameter
        (Mode           => Request_Parameters.Required,
         Parameter_Name => "msg",
         Validate_As    => Request_Parameters.Non_Empty_String);

      Instance.Validate_Request_Parameters;

      if Instance.Valid_Request_Parameters then
         return Instance.Parameter ("msg");
      else
         return "";
      end if;
   end Get_Message;

   ----------------
   --  Info_Log  --
   ----------------

   procedure Info_Log
     (Instance : in out Response.Object)
   is
      use System_Message;

      Msg : constant String := Get_Message (Instance);
   begin
      if Msg'Length > 0 then
         Info.Client_Info (Message         => Msg,
                           Response_Object => Instance);
      end if;
   end Info_Log;

end Handlers.Log;
