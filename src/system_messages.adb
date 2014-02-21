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

with Ada.Text_IO,
     Ada.Directories;

with Configuration;

package body System_Messages is
   use Configuration;

   Access_Logfile  : Ada.Text_IO.File_Type;
   Error_Logfile   : Ada.Text_IO.File_Type;
   Mute_Access_Log : Boolean := False;
   Mute_Error_Log  : Boolean := False;

   ------------------
   --  Access_Log  --
   ------------------

   procedure Access_Log (Message : in String) is
   begin
      if not Mute_Access_Log then
         Ada.Text_IO.Put_Line (File => Access_Logfile,
                               Item => Message);
         Ada.Text_IO.Flush (Access_Logfile);
      end if;
   end Access_Log;

   -----------------------
   --  Close_Log_Files  --
   -----------------------

   procedure Close_Log_Files is
      use Ada.Text_IO;
   begin
      Close (File => Access_Logfile);
      Close (File => Error_Logfile);
   end Close_Log_Files;

   ----------------
   --  Critical  --
   ----------------

   procedure Critical (Message : in String;
                       Context : in String) is
   begin
      if Loglevel <= Critical then
         Ada.Text_IO.Put_Line (Loglevels'Image (Critical) & Separator &
                                 Context & Separator & Message);
      end if;
   end Critical;

   --------------------------
   --  Critical_Exception  --
   --------------------------

   procedure Critical_Exception
     (Message : in String;
      Event   : in Ada.Exceptions.Exception_Occurrence;
      Context : in String) is
   begin
      Ada.Text_IO.Put_Line (Loglevels'Image (Critical) &
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
      if Loglevel <= Debug then
         Ada.Text_IO.Put_Line (Loglevels'Image (Debug) & Separator &
                                 Context & Separator & Message);
      end if;
   end Debug;

   -------------
   --  Error  --
   -------------

   procedure Error (Message : in String;
                    Context : in String) is
   begin
      if Loglevel <= Error then
         Ada.Text_IO.Put_Line (Loglevels'Image (Error) & Separator &
                                 Context & Separator & Message);
      end if;
   end Error;

   -----------------
   --  Error_Log  --
   -----------------

   procedure Error_Log (Message : in String) is
   begin
      if not Mute_Error_Log then
         Ada.Text_IO.Put_Line (File => Error_Logfile,
                               Item => Message);
         Ada.Text_IO.Flush (Error_Logfile);
      end if;
   end Error_Log;

   -------------
   --  Fixme  --
   -------------

   procedure Fixme (Message : in String;
                    Context : in String) is
   begin
      Ada.Text_IO.Put_Line (Loglevels'Image (Fixme) & Separator &
                              Context & Separator & Message);
   end Fixme;

   -------------------
   --  Information  --
   -------------------

   procedure Information (Message : in String;
                          Context : in String) is
   begin
      if Loglevel <= Information then
         Ada.Text_IO.Put_Line (Loglevels'Image (Information) & Separator &
                                 Context & Separator & Message);
      end if;
   end Information;

   ----------------------
   --  Open_Log_Files  --
   ----------------------

   procedure Open_Log_Files is
      use Ada.Text_IO;
      use Ada.Directories;

      Context : constant String := Package_Name & ".Open_Log_Files";

   begin
      declare
      begin
         if Exists (Config.Get (Access_Log)) then

            Open (File => Access_Logfile,
                  Mode => Append_File,
                  Name => Config.Get (Access_Log));
         else
            Create (File => Access_Logfile,
                    Mode => Out_File,
                    Name => Config.Get (Access_Log));
         end if;

            System_Messages.Information
              (Message => "Opened access log: " &
                 Config.Get (Access_Log),
               Context => Context);
      exception
         when others =>
            System_Messages.Error
              (Message => "Failed to open access log: " &
                 Config.Get (Access_Log),
               Context => Context);
            Mute_Access_Log := True;
      end;

      declare
      begin
         if Exists (Config.Get (Error_Log)) then
            Open (File => Error_Logfile,
                  Mode => Append_File,
                  Name => Config.Get (Error_Log));
         else
            Create (File => Error_Logfile,
                    Mode => Out_File,
                    Name => Config.Get (Error_Log));
         end if;
         System_Messages.Information
              (Message => "Opened error log: " &
                 Config.Get (Error_Log),
               Context => Context);
      exception
         when others =>
            System_Messages.Error
              (Message => "Failed to open error log: " &
                 Config.Get (Error_Log),
               Context => Context);
            Mute_Error_Log := True;
      end;

   end Open_Log_Files;

end System_Messages;
