-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2013-, AdaHeads K/S                     --
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

with Ada.Command_Line,
     Ada.Exceptions,
     Ada.Text_IO;

with Access_Generator;

procedure Not_Null_Check is
   use Ada.Command_Line,
       Ada.Text_IO,
       Access_Generator;

   procedure Failed (Message : in     String);
   procedure Failed (Message : in     String) is
   begin
      if Argument (1) = "--set-exit-status" then
         Set_Exit_Status (Failure);
      end if;

      Put_Line (Standard_Error, Message);
   end Failed;

   Object : not null Reference := Initialise;
begin
   loop
      exit when Object = null;
      Put_Line (Image (Object.all));
      Object := Next;
   end loop;

   Failed
     ("Expected an exception due to null assignment to ""not null"" object.");
exception
   when Constraint_Error =>
      Put_Line ("An exception was raised as expected.");
   when E : others =>
      Failed (Message => "An exception was raised: " &
                         Ada.Exceptions.Exception_Information (E));
end Not_Null_Check;
