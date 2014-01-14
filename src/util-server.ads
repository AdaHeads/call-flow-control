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

with AWS.Config;
with AWS.Exceptions;
with AWS.Server;
with AWS.Dispatchers;

package Util.Server is

   Package_Name : constant String := "Util.Server";

   type HTTP is tagged limited private;

   function Create
     (Unexpected : in AWS.Exceptions.Unexpected_Exception_Handler)
      return HTTP;
   --  Create a HTTP object. This contains an AWS HTTP server that is
   --  configured according to the configuration settings found in
   --  ./configuration/config.ini.
   --
   --  Unexcpected
   --    Access to the unexpected exception handler.
   --
   --  NOTE:
   --  Yolk currently only supports starting one AWS server. Successive calls
   --  to Create are ignored. Only one HTTP object can be active at any given
   --  point.

   procedure Start
     (WS          : in out HTTP;
      Dispatchers : in     AWS.Dispatchers.Handler'Class);
   --  Start the AWS HTTP(S) server.
   --
   --  Dispatchers
   --    Dispatcher services for the server.

   procedure Stop
     (WS : in out HTTP);
   --  Stop the AWS HTTP(S) server.

private

   type HTTP is tagged limited
      record
         Handle_The_Unexpected : AWS.Exceptions.Unexpected_Exception_Handler;
         Web_Server            : AWS.Server.HTTP;
         Web_Server_Config     : AWS.Config.Object;
      end record;

end Util.Server;
