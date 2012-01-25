-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                  LDAP                                     --
--                                                                           --
--                                  SPEC                                     --
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

with Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with AWS.LDAP.Client;

package LDAP is
private

   use Ada.Calendar;
   use Ada.Containers;
   use Ada.Strings.Unbounded;

   type Server is
      record
         LDAP_Dir    : AWS.LDAP.Client.Directory;
         Host        : Unbounded_String;
         Password    : Unbounded_String;
         Port        : Positive;
         User_DN     : Unbounded_String;
         Death_Stamp : Time;
      end record;

   package Connection_Unit_List is new Doubly_Linked_Lists (Server);

   type Server_Store is
      record
         Initialized  : Boolean;
         Live_List    : Connection_Unit_List.List;
         Dead_List    : Connection_Unit_List.List;
      end record;

   Null_Store : constant Server_Store := (False,
                                   Connection_Unit_List.Empty_List,
                                   Connection_Unit_List.Empty_List);

   function Error_Handler
     (Event       : in Ada.Exceptions.Exception_Occurrence;
      LDAP_Values : in String)
      return String;
   --  Handler exceptions raised due to bad LDAP search parameters.

   function Get_Directory
     (A_Server : in Server)
      return AWS.LDAP.Client.Directory;

   procedure Initialize_Server_Store
     (Store : out Server_Store);
   --  STUFF!

   function Pop_Server
     return Server;
   --  Return a thread specific Server record.

   procedure Push_Server
     (A_Server : in out Server;
      Is_Alive : in     Boolean := True);
   --  STUFF!

end LDAP;
