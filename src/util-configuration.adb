-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2014-, AdaHeads K/S                     --
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

with AWS.Config.Set;

package body Util.Configuration is

   ---------------------------
   -- Get_AWS_Configuration --
   ---------------------------

   function Get_AWS_Configuration return AWS.Config.Object
   is
      Object : AWS.Config.Object;
   begin
      AWS.Config.Set.Accept_Queue_Size
        (O     => Object,
         Value => Config.Get (Accept_Queue_Size));

      AWS.Config.Set.Admin_Password
        (O     => Object,
         Value => Config.Get (Admin_Password));

      AWS.Config.Set.Admin_URI
        (O     => Object,
         Value => Config.Get (Admin_URI));

      AWS.Config.Set.Case_Sensitive_Parameters
        (O     => Object,
         Value => Config.Get (Case_Sensitive_Parameters));

      AWS.Config.Set.Certificate
        (O        => Object,
         Filename => Config.Get (Certificate));

      AWS.Config.Set.Certificate_Required
        (O     => Object,
         Value => Config.Get (Certificate_Required));

      AWS.Config.Set.Check_URL_Validity
        (O     => Object,
         Value => Config.Get (Check_URL_Validity));

      AWS.Config.Set.Cleaner_Client_Data_Timeout
        (O     => Object,
         Value => Config.Get (Cleaner_Client_Data_Timeout));

      AWS.Config.Set.Cleaner_Client_Header_Timeout
        (O     => Object,
         Value => Config.Get (Cleaner_Client_Header_Timeout));

      AWS.Config.Set.Cleaner_Server_Response_Timeout
        (O     => Object,
         Value => Config.Get (Cleaner_Server_Response_Timeout));

      AWS.Config.Set.Cleaner_Wait_For_Client_Timeout
        (O     => Object,
         Value => Config.Get (Cleaner_Wait_For_Client_Timeout));

      AWS.Config.Set.Context_Lifetime
        (Value => Config.Get (Context_Lifetime));

      AWS.Config.Set.CRL_File
        (O        => Object,
         Filename => Config.Get (CRL_File));

      AWS.Config.Set.Directory_Browser_Page
        (O     => Object,
         Value => Config.Get (System_Templates_Path) & "/aws_directory.tmpl");

      AWS.Config.Set.Exchange_Certificate
        (O     => Object,
         Value => Config.Get (Exchange_Certificate));

      AWS.Config.Set.Force_Client_Data_Timeout
        (O     => Object,
         Value => Config.Get (Force_Client_Data_Timeout));

      AWS.Config.Set.Force_Client_Header_Timeout
        (O     => Object,
         Value => Config.Get (Force_Client_Header_Timeout));

      AWS.Config.Set.Force_Server_Response_Timeout
        (O     => Object,
         Value => Config.Get (Force_Server_Response_Timeout));

      AWS.Config.Set.Force_Wait_For_Client_Timeout
        (O     => Object,
         Value => Config.Get (Force_Wait_For_Client_Timeout));

      AWS.Config.Set.Free_Slots_Keep_Alive_Limit
        (O     => Object,
         Value => Config.Get (Free_Slots_Keep_Alive_Limit));

      AWS.Config.Set.Hotplug_Port
        (O     => Object,
         Value => Config.Get (Hotplug_Port));

      AWS.Config.Set.Keep_Alive_Force_Limit
        (O     => Object,
         Value => Config.Get (Keep_Alive_Force_Limit));

      AWS.Config.Set.Key
        (O        => Object,
         Filename => Config.Get (Key));

      AWS.Config.Set.Line_Stack_Size
        (O     => Object,
         Value => Config.Get (Line_Stack_Size));

      AWS.Config.Set.Log_Extended_Fields
        (O     => Object,
         Value => Config.Get (Log_Extended_Fields));

      AWS.Config.Set.Max_Concurrent_Download
        (Value => Config.Get (Max_Concurrent_Download));

      AWS.Config.Set.Max_Connection
        (O     => Object,
         Value => Config.Get (Max_Connection));

      AWS.Config.Set.Max_POST_Parameters
        (O     => Object,
         Value => Config.Get (Max_POST_Parameters));

      AWS.Config.Set.Max_WebSocket_Handler
        (Value => Config.Get (Max_WebSocket_Handler));

      AWS.Config.Set.MIME_Types
        (O     => Object,
         Value => Config.Get (MIME_Types_File));

      AWS.Config.Set.Protocol_Family
        (O     => Object,
         Value => Config.Get (Protocol_Family));

      AWS.Config.Set.Receive_Timeout
        (O     => Object,
         Value => Config.Get (Receive_Timeout));

      AWS.Config.Set.Reuse_Address
        (O     => Object,
         Value => Config.Get (Reuse_Address));

      AWS.Config.Set.Security
        (O     => Object,
         Value => Config.Get (Security));

      AWS.Config.Set.Security_Mode
        (O    => Object,
         Mode => Config.Get (Security_Mode));

      AWS.Config.Set.Send_Timeout
        (O     => Object,
         Value => Config.Get (Send_Timeout));

      AWS.Config.Set.Server_Host
        (O     => Object,
         Value => Config.Get (Server_Host));

      AWS.Config.Set.Server_Name
        (O     => Object,
         Value => Config.Get (Server_Name));

      AWS.Config.Set.Server_Port
        (O     => Object,
         Value => Config.Get (Server_Port));

      AWS.Config.Set.Server_Priority
        (O     => Object,
         Value => Config.Get (Server_Priority));

      AWS.Config.Set.Service_Priority
        (Value => Config.Get (Service_Priority));

      AWS.Config.Set.Session
        (O     => Object,
         Value => Config.Get (Session));

      AWS.Config.Set.Session_Cleanup_Interval
        (Value => Config.Get (Session_Cleanup_Interval));

      AWS.Config.Set.Session_Cleaner_Priority
        (Value => Config.Get (Session_Cleaner_Priority));

      AWS.Config.Set.Session_Id_Length
        (Value => Config.Get (Session_Id_Length));

      AWS.Config.Set.Session_Lifetime
        (Value => Config.Get (Session_Lifetime));

      AWS.Config.Set.Session_Name
        (O     => Object,
         Value => Config.Get (Session_Name));

      AWS.Config.Set.Status_Page
        (O     => Object,
         Value => Config.Get (Status_Page));

      AWS.Config.Set.Transient_Cleanup_Interval
        (Value => Config.Get (Transient_Cleanup_Interval));

      AWS.Config.Set.Transient_Lifetime
        (Value => Config.Get (Transient_Lifetime));

      AWS.Config.Set.Trusted_CA
        (O        => Object,
         Filename => Config.Get (Trusted_CA));

      AWS.Config.Set.Upload_Directory
        (O     => Object,
         Value => Config.Get (Upload_Directory));

      AWS.Config.Set.Upload_Size_Limit
        (O     => Object,
         Value => Config.Get (Upload_Size_Limit));

      AWS.Config.Set.WebSocket_Message_Queue_Size
        (Value => Config.Get (WebSocket_Message_Queue_Size));

      AWS.Config.Set.WebSocket_Origin
        (Value => Config.Get (WebSocket_Origin));

      AWS.Config.Set.WebSocket_Priority
        (Value => Config.Get (WebSocket_Priority));

      AWS.Config.Set.WWW_Root
        (O     => Object,
         Value => Config.Get (WWW_Root));

      return Object;
   end Get_AWS_Configuration;

begin
   Config.Load_File;

end Util.Configuration;
