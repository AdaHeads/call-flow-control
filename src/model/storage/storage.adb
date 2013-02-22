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

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

with Storage.Connection;
with System_Message.Critical;

package body Storage is

   Database_Error : exception;
   --  Is raised if we can't complete a query for one reason or another.

   function Trim
     (Source : in String)
      return String;
   --  Trim Source string on both sides. This will clear away \n also. This
   --  function is here because the errors thrown by PostgreSQL is postfixed
   --  with a \n which we must remove before sending the message to syslogd.

   ----------------------------
   --  Process_Select_Query  --
   ----------------------------

   procedure Process_Select_Query
     (Process_Element    : not null access procedure (E : in Element);
      Prepared_Statement : in GNATCOLL.SQL.Exec.Prepared_Statement;
      Query_Parameters   : in GNATCOLL.SQL.Exec.SQL_Parameters)
   is
      use Connection;
      use GNATCOLL.SQL;
      use GNATCOLL.SQL.Exec;
      use System_Message;

      Cursor     : Database_Cursor;
      Connection : Database_Connection;
   begin
      Connection := Get_Connection;

      Cursor.Fetch (Connection,
                    Prepared_Statement,
                    Query_Parameters);

      if Connection.Success then
         while Cursor.Has_Row loop
            Process_Element (Cursor_To_Element (Cursor));
            --  Note that this loop will go on forever if the Cursor_To_Element
            --  function does not properly handle moving the cursor forward.
         end loop;
      else
         Critical.Lost_Database_Connection
           (Message => Trim (Exec.Error (Connection)));
         raise Database_Error;
      end if;
   end Process_Select_Query;

   ------------
   --  Trim  --
   ------------

   function Trim
     (Source : in String)
      return String
   is
      use Ada.Strings;
   begin
      if Source (Source'Last) = Ada.Characters.Latin_1.LF then
         return Source (Source'First ..  Source'Last - 1);
      end if;

      return Fixed.Trim (Source, Both);
   end Trim;

end Storage;
