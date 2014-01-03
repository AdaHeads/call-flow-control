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

with Ada.Text_IO;

package body System_Messages is

   ----------------
   --  Critical  --
   ----------------

   procedure Critical (Message : in String;
                       Context : in String) is
   begin
      Ada.Text_IO.Put_Line (Message_Type'Image (Critical) & Separator &
                              Context & Separator & Message);
   end Critical;

   --------------
   --  Debug   --
   --------------

   procedure Debug (Message : in String;
                    Context : in String) is
   begin
      Ada.Text_IO.Put_Line (Message_Type'Image (Debug) & Separator &
                              Context & Separator & Message);
   end Debug;

   -------------
   --  Error  --
   -------------

   procedure Error (Message : in String;
                    Context : in String) is
   begin
      Ada.Text_IO.Put_Line (Message_Type'Image (Error) & Separator &
                              Context & Separator & Message);
   end Error;

   -------------
   --  Fixme  --
   -------------

   procedure Fixme (Message : in String;
                       Context : in String) is
   begin
      Ada.Text_IO.Put_Line (Message_Type'Image (Fixme) & Separator &
                              Context & Separator & Message);
   end Fixme;

   -------------------
   --  Information  --
   -------------------

   procedure Information (Message : in String;
                          Context : in String) is
   begin
      Ada.Text_IO.Put_Line (Message_Type'Image (Information) & Separator &
                              Context & Separator & Message);
   end Information;

end System_Messages;
