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

--  Default configuration settings for a Yolk application.
--  Keys postfixed with "--  AWS" are keys that are used specifically by AWS.
--  Other keys are for Yolk specific configuration.

with Ada.Strings;
with Ada.Strings.Unbounded;
with AWS.Config;
with AWS.Default;
with System;
with Util.Config_File_Parser;

package Util.Configuration is

   use Ada.Strings;
   use Ada.Strings.Unbounded;

   function U
     (S : in String)
      return Unbounded_String
      renames To_Unbounded_String;

   type Keys is (Accept_Queue_Size, --  AWS
                 Admin_Password, --  AWS
                 Admin_URI, --  AWS
                 Alert_Log_Activate,
                 Alert_Syslog_Facility_Level,
                 AWS_Access_Log_Activate,
                 AWS_Access_Syslog_Facility_Level,
                 AWS_Error_Log_Activate,
                 AWS_Error_Syslog_Facility_Level,
                 Case_Sensitive_Parameters, --  AWS
                 Certificate, --  AWS
                 Certificate_Required, --  AWS
                 Check_URL_Validity, --  AWS
                 Cleaner_Client_Data_Timeout, --  AWS
                 Cleaner_Client_Header_Timeout, --  AWS
                 Cleaner_Server_Response_Timeout, --  AWS
                 Cleaner_Wait_For_Client_Timeout, --  AWS
                 Compress_Static_Content,
                 Compress_Static_Content_Minimum_File_Size,
                 Compressed_Static_Content_Cache,
                 Compressed_Static_Content_Max_Age,
                 Context_Lifetime, --  AWS
                 Critical_Log_Activate,
                 Critical_Syslog_Facility_Level,
                 CRL_File, --  AWS
                 Debug_Log_Activate,
                 Debug_Syslog_Facility_Level,
                 Emergency_Log_Activate,
                 Emergency_Syslog_Facility_Level,
                 Error_Log_Activate,
                 Error_Syslog_Facility_Level,
                 Exchange_Certificate, --  AWS
                 Force_Client_Data_Timeout, --  AWS
                 Force_Client_Header_Timeout, --  AWS
                 Force_Server_Response_Timeout, --  AWS
                 Force_Wait_For_Client_Timeout, --  AWS
                 Free_Slots_Keep_Alive_Limit, --  AWS
                 Handler_CSS,
                 Handler_GIF,
                 Handler_HTML,
                 Handler_ICO,
                 Handler_JPG,
                 Handler_JS,
                 Handler_PNG,
                 Handler_SVG,
                 Handler_XML,
                 Handler_XSL,
                 Hotplug_Port, --  AWS
                 Immediate_Flush,
                 Info_Log_Activate,
                 Info_Syslog_Facility_Level,
                 Keep_Alive_Close_Limit, --  AWS
                 Keep_Alive_Force_Limit, --  AWS
                 Key, --  AWS
                 Line_Stack_Size, --  AWS
                 Load_MIME_Types_File,
                 Log_Extended_Fields, --  AWS
                 Max_Concurrent_Download, --  AWS
                 Max_Connection, --  AWS
                 Max_POST_Parameters, --  AWS
                 Max_WebSocket_Handler, --  AWS
                 MIME_Types_File, --  AWS
                 Notice_Log_Activate,
                 Notice_Syslog_Facility_Level,
                 Protocol_Family, --  AWS
                 Receive_Timeout, --  AWS
                 Reuse_Address, --  AWS
                 Security, --  AWS
                 Security_Mode, --  AWS
                 Send_Timeout, --  AWS
                 Server_Host, --  AWS
                 Server_Name, --  AWS
                 Server_Port, --  AWS
                 Server_Priority, --  AWS
                 Service_Priority, --  AWS
                 Session, --  AWS
                 Session_Cleanup_Interval, --  AWS
                 Session_Cleaner_Priority, --  AWS
                 Session_Data_File,
                 Session_Id_Length, --  AWS
                 Session_Lifetime, --  AWS
                 Session_Name, --  AWS
                 SQL_Log_Activate,
                 SQL_Syslog_Facility_Level,
                 SQL_Cache_Log_Activate,
                 SQL_Cache_Syslog_Facility_Level,
                 SQL_Error_Log_Activate,
                 SQL_Error_Syslog_Facility_Level,
                 SQL_Select_Log_Activate,
                 SQL_Select_Syslog_Facility_Level,
                 Start_WebSocket_Servers,
                 Status_Page, --  AWS
                 System_Templates_Path,
                 Transient_Cleanup_Interval, --  AWS
                 Transient_Lifetime, --  AWS
                 Trusted_CA, --  AWS
                 Upload_Directory, --  AWS
                 Upload_Size_Limit, --  AWS
                 Warning_Log_Activate,
                 Warning_Syslog_Facility_Level,
                 WebSocket_Message_Queue_Size, --  AWS
                 WebSocket_Origin, --  AWS
                 WebSocket_Priority, --  AWS
                 WWW_Root, --  AWS
                 Yolk_User);

   type Defaults_Array is array (Keys) of Unbounded_String;

   Default_Values : constant Defaults_Array :=
                      (Accept_Queue_Size
                       => U ("128"),
                       Admin_Password
                       => U ("0ac9c9d0c0b1ee058b65ae70c9aeb3a7"),
                       Admin_URI
                       => U ("/status"),
                       Alert_Log_Activate
                       => U ("True"),
                       Alert_Syslog_Facility_Level
                       => U ("user:alert"),
                       AWS_Access_Log_Activate
                       => U ("True"),
                       AWS_Access_Syslog_Facility_Level
                       => U ("user:info"),
                       AWS_Error_Log_Activate
                       => U ("True"),
                       AWS_Error_Syslog_Facility_Level
                       => U ("user:error"),
                       Case_Sensitive_Parameters
                       => U (Boolean'Image
                         (AWS.Default.Case_Sensitive_Parameters)),
                       Certificate
                       => U ("certificates/aws.pem"),
                       Certificate_Required
                       => U (Boolean'Image (AWS.Default.Certificate_Required)),
                       Check_URL_Validity
                       => U (Boolean'Image (AWS.Default.Check_URL_Validity)),
                       Cleaner_Client_Data_Timeout
                       => Trim (U (Duration'Image
                         (AWS.Default.Cleaner_Client_Data_Timeout)), Left),
                       Cleaner_Client_Header_Timeout
                       => Trim (U (Duration'Image
                         (AWS.Default.Cleaner_Client_Header_Timeout)), Left),
                       Cleaner_Server_Response_Timeout
                       => Trim (U (Duration'Image
                         (AWS.Default.Cleaner_Server_Response_Timeout)), Left),
                       Cleaner_Wait_For_Client_Timeout
                       => Trim (U (Duration'Image
                         (AWS.Default.Cleaner_Wait_For_Client_Timeout)), Left),
                       Compress_Static_Content
                       => U ("False"),
                       Compress_Static_Content_Minimum_File_Size
                       => U ("200"),
                       Compressed_Static_Content_Cache
                       => U ("static_content/compressed_cache"),
                       Compressed_Static_Content_Max_Age
                       => U ("86400"),
                       Context_Lifetime
                       => Trim (U (Duration'Image
                         (AWS.Default.Context_Lifetime)), Left),
                       Critical_Log_Activate
                       => U ("True"),
                       Critical_Syslog_Facility_Level
                       => U ("user:critical"),
                       CRL_File
                       => U (AWS.Default.CRL_File),
                       Debug_Log_Activate
                       => U ("True"),
                       Debug_Syslog_Facility_Level
                       => U ("user:debug"),
                       Emergency_Log_Activate
                       => U ("True"),
                       Emergency_Syslog_Facility_Level
                       => U ("user:emergency"),
                       Error_Log_Activate
                       => U ("True"),
                       Error_Syslog_Facility_Level
                       => U ("user:error"),
                       Exchange_Certificate
                       => U (Boolean'Image (AWS.Default.Exchange_Certificate)),
                       Force_Client_Data_Timeout
                       => Trim (U (Duration'Image
                         (AWS.Default.Force_Client_Data_Timeout)), Left),
                       Force_Client_Header_Timeout
                       => Trim (U (Duration'Image
                         (AWS.Default.Force_Client_Header_Timeout)), Left),
                       Force_Server_Response_Timeout
                       => Trim (U (Duration'Image
                         (AWS.Default.Force_Server_Response_Timeout)), Left),
                       Force_Wait_For_Client_Timeout
                       => Trim (U (Duration'Image
                         (AWS.Default.Force_Wait_For_Client_Timeout)), Left),
                       Free_Slots_Keep_Alive_Limit
                       => Trim (U (Natural'Image
                         (AWS.Default.Free_Slots_Keep_Alive_Limit)), Left),
                       Handler_CSS
                       => U (".*\.css$"),
                       Handler_GIF
                       => U (".*\.gif$"),
                       Handler_HTML
                       => U (".*\.html$"),
                       Handler_ICO
                       => U (".*\.ico$"),
                       Handler_JPG
                       => U (".*\.jpg$"),
                       Handler_JS
                       => U (".*\.js$"),
                       Handler_PNG
                       => U (".*\.png$"),
                       Handler_SVG
                       => U (".*\.svg$"),
                       Handler_XML
                       => U (".*\.xml$"),
                       Handler_XSL
                       => U (".*\.xsl$"),
                       Hotplug_Port
                       => Trim (U (Positive'Image
                         (AWS.Default.Hotplug_Port)), Left),
                       Immediate_Flush
                       => U ("False"),
                       Info_Log_Activate
                       => U ("True"),
                       Info_Syslog_Facility_Level
                       => U ("user:info"),
                       Keep_Alive_Close_Limit
                       => Trim (U (Natural'Image
                         (AWS.Default.Keep_Alive_Close_Limit)), Left),
                       Keep_Alive_Force_Limit
                       => Trim (U (Natural'Image
                         (AWS.Default.Keep_Alive_Force_Limit)), Left),
                       Key
                       => U (AWS.Default.Key),
                       Line_Stack_Size
                       => U ("16#150_000#"),
                       Load_MIME_Types_File
                       => U ("False"),
                       Log_Extended_Fields
                       => U (""),
                       Max_Concurrent_Download
                       => Trim (U (Positive'Image
                         (AWS.Default.Max_Concurrent_Download)), Left),
                       Max_Connection
                       => Trim (U (Positive'Image
                         (AWS.Default.Max_Connection)), Left),
                       Max_POST_Parameters
                       => Trim (U (Positive'Image
                         (AWS.Default.Max_POST_Parameters)), Left),
                       Max_WebSocket_Handler
                       => U ("5"),
                       MIME_Types_File
                       => U ("configuration/aws.mime"),
                       Notice_Log_Activate
                       => U ("True"),
                       Notice_Syslog_Facility_Level
                       => U ("user:notice"),
                       Protocol_Family
                       => U ("Family_Unspec"),
                       Receive_Timeout
                       => Trim (U (Duration'Image
                         (AWS.Default.Receive_Timeout)), Left),
                       Reuse_Address
                       => U (Boolean'Image (True)),
                       Security
                       => U (Boolean'Image (AWS.Default.Security)),
                       Security_Mode
                       => U (AWS.Default.Security_Mode),
                       Send_Timeout
                       => Trim (U (Duration'Image
                         (AWS.Default.Send_Timeout)), Left),
                       Server_Host
                       => U (""),
                       Server_Name
                       => U ("Yolk"),
                       Server_Port
                       => U ("4242"),
                       Server_Priority
                       => Trim (U (System.Priority'Image
                         (AWS.Default.Server_Priority)), Left),
                       Service_Priority
                       => Trim (U (System.Priority'Image
                         (AWS.Default.Service_Priority)), Left),
                       Session
                       => U ("True"),
                       Session_Cleanup_Interval
                       => Trim (U (Duration'Image
                         (AWS.Default.Session_Cleanup_Interval)), Left),
                       Session_Cleaner_Priority
                       => Trim (U (System.Priority'Image
                         (AWS.Default.Session_Cleaner_Priority)), Left),
                       Session_Data_File
                       => U ("session/session.data"),
                       Session_Id_Length
                       => Trim (U (Positive'Image
                         (AWS.Default.Session_Id_Length)), Left),
                       Session_Lifetime
                       => Trim (U (Duration'Image
                         (AWS.Default.Session_Lifetime)), Left),
                       Session_Name
                       => U ("Yolk"),
                       SQL_Log_Activate
                       => U ("True"),
                       SQL_Syslog_Facility_Level
                       => U ("user:info"),
                       SQL_Cache_Log_Activate
                       => U ("True"),
                       SQL_Cache_Syslog_Facility_Level
                       => U ("user:info"),
                       SQL_Error_Log_Activate
                       => U ("True"),
                       SQL_Error_Syslog_Facility_Level
                       => U ("user:error"),
                       SQL_Select_Log_Activate
                       => U ("True"),
                       SQL_Select_Syslog_Facility_Level
                       => U ("user:info"),
                       Start_WebSocket_Servers
                       => U ("True"),
                       Status_Page
                       => U ("templates/system/aws_status.thtml"),
                       System_Templates_Path
                       => U ("templates/system"),
                       Transient_Cleanup_Interval
                       => Trim (U (Duration'Image
                         (AWS.Default.Transient_Cleanup_Interval)), Left),
                       Transient_Lifetime
                       => Trim (U (Duration'Image
                         (AWS.Default.Transient_Lifetime)), Left),
                       Trusted_CA
                       => U (AWS.Default.Trusted_CA),
                       Upload_Directory
                       => U ("uploads"),
                       Upload_Size_Limit
                       => U ("16#500_000#"),
                       Warning_Log_Activate
                       => U ("True"),
                       Warning_Syslog_Facility_Level
                       => U ("user:warning"),
                       WebSocket_Message_Queue_Size
                       => Trim (U (Positive'Image
                         (AWS.Default.WebSocket_Message_Queue_Size)), Left),
                       WebSocket_Origin
                       => U (""),
                       WebSocket_Priority
                       => Trim (U (System.Priority'Image
                         (AWS.Default.WebSocket_Priority)), Left),
                       WWW_Root
                       => U ("static_content"),
                       Yolk_User
                       => U ("thomas"));
   --  Default values for the configuration Keys. These values can be over-
   --  written by the configuration file given when instantiating the
   --  Config_File_Parser generic.

   package Config is new Config_File_Parser
     (Key_Type            => Keys,
      Defaults_Array_Type => Defaults_Array,
      Defaults            => Default_Values,
      Config_File         => "conf/config.ini");

   function Get_AWS_Configuration return AWS.Config.Object;
   --  Load the AWS relevant configuration settings from the config.ini file.

end Util.Configuration;
