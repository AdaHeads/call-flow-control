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

   No_LDAP_Server_Available : exception;

   type Server is
      record
         LDAP_Dir    : AWS.LDAP.Client.Directory;
         Host        : Unbounded_String;
         Port        : Positive;
         Password    : Unbounded_String;
         Base_DN     : Unbounded_String;
         User_DN     : Unbounded_String;
         Death_Stamp : Time;
         Coffin_Time : Duration;
      end record;

   package Server_List is new Doubly_Linked_Lists (Server);
   use Server_List;

   type Server_Store is
      record
         Initialized  : Boolean;
         Live_List    : List;
         Dead_List    : List;
      end record;

   Servers_From_JSON : List := Empty_List;
   --  Contains Server objects generated from the JSON server file. Servers are
   --  addded by the Initialize procedure.

   Null_Store : constant Server_Store := (False, Empty_List, Empty_List);

   function Error_Handler
     (Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String)
      return String;
   --  Handles exceptions raised due to bad LDAP search parameters. Returns a
   --  JSON String that can be sent to the client.

   procedure Error_Handler
     (Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String);
   --  Same as the Error_Handler function, except no JSON String is made. This
   --  simply logs the error to the Error trace.

   function Escape
     (Query : in String;
      Is_DN : in Boolean := False)
      return String;
   --  Escape *()\NUL for filters:
   --    *   -> \2a
   --    (   -> \28
   --    )   -> \29
   --    \   -> \5c
   --    NUL -> \00
   --
   --  Escape ,=+<>;\"# for DN's:
   --    , -> \2c
   --    = -> \3d
   --    + -> \2b
   --    < -> \3c
   --    > -> \3e
   --    ; -> \3b
   --    \ -> \5c
   --    " -> \22
   --    # -> \23

   function Get_Base_DN
     (A_Server : in Server)
     return String;

   procedure Initialize_Server_Store
     (Store : out Server_Store);
   --  STUFF!

   procedure Put_Server
     (A_Server : in out Server;
      Is_Alive : in     Boolean := True);
   --  STUFF!

   function Take_Server
     return Server;
   --  Return a thread specific Server record.

end LDAP;
