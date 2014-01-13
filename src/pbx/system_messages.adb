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

   ------------------
   --  Access_Log  --
   ------------------

   procedure Access_Log (Message : in String) is
   begin
      Ada.Text_IO.Put_Line ("ACCESS LOG" & Separator & Message);
   end Access_Log;

   ----------------
   --  Critical  --
   ----------------

   procedure Critical (Message : in String;
                       Context : in String) is
   begin
      Ada.Text_IO.Put_Line (Message_Type'Image (Critical) & Separator &
                              Context & Separator & Message);
   end Critical;

   --------------------------
   --  Critical_Exception  --
   --------------------------

   procedure Critical_Exception
     (Message : in String;
      Event   : in Ada.Exceptions.Exception_Occurrence;
      Context : in String) is
   begin
      Ada.Text_IO.Put_Line (Message_Type'Image (Critical) &
                              Separator &
                              Context   &
                              Separator &
                              Message   &
                              Ada.Exceptions.Exception_Information (Event));
   end Critical_Exception;

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

   -----------------
   --  Error_Log  --
   -----------------

   procedure Error_Log (Message : in String) is
   begin
      Ada.Text_IO.Put_Line ("ERROR LOG" & Separator & Message);
   end Error_Log;

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
