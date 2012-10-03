-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                  Errors                                   --
--                                                                           --
--                                  BODY                                     --
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

with GNATCOLL.JSON;
with HTTP_Codes;
with Yolk.Log;

package body Errors is

   use HTTP_Codes;

   function U
     (S : in String)
      return Unbounded_String
      renames To_Unbounded_String;

   function Build_Error_Record
     (Description : in String;
      Status      : in String;
      Status_Code : in AWS.Messages.Status_Code)
      return Error_Record;
   --  TODO: Write comment.

   --------------------------
   --  Build_Error_Record  --
   --------------------------

   function Build_Error_Record
     (Description     : in String;
      Status          : in String;
      Status_Code     : in AWS.Messages.Status_Code)
      return Error_Record
   is
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Log;
   begin
      return (JSON        => Null_JSON_String,
              Description => U (Description),
              Status      => U (Status),
              Status_Code => Status_Code);
   end Build_Error_Record;

   Error_Messages : array (Error_Type) of Error_Record :=
                      (Database_Error      => Build_Error_Record
                         (Description     => "",
                          Status          => "database error",
                          Status_Code     => Server_Error),
                       GET_Parameter_Error => Build_Error_Record
                         (Description     => "",
                          Status          => "parameter error",
                          Status_Code     => Bad_Request));

   ------------
   --  JSON  --
   ------------

   function JSON
     (Err : in Error_Record)
      return Common.JSON_String
   is
   begin
      return Err.JSON;
   end JSON;

   ---------------------
   --  Log_Exception  --
   ---------------------

   function Log_Exception
     (Err     : in Error_Type;
      Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String)
      return Error_Record
   is
      use Ada.Exceptions;
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Log;

      E_Name : constant String := Exception_Name (Event);
      E_Msg  : constant String := Exception_Message (Event);
      J      : JSON_Value;
   begin
      Trace (Error, Error_Type'Image (Err)
             & " - "
             & E_Name
             & " - "
             & E_Msg
             & " - "
             & Message);

      if Message /= "" then
         Append (Source   => Error_Messages (Err).Description,
                 New_Item => " - " & Message);
      end if;

      J := Create_Object;

      J.Set_Field (Field_Name => "description",
                   Field      => Error_Messages (Err).Description);
      J.Set_Field (Field_Name => "status",
                   Field      => Error_Messages (Err).Status);

      Error_Messages (Err).JSON := To_JSON_String (J.Write);

      return Error_Messages (Err);
   end Log_Exception;

   ---------------------
   --  Log_Exception  --
   ---------------------

   function Log_Exception
     (Err     : in Error_Type;
      Message : in String)
      return Error_Record
   is
      use Common;
      use GNATCOLL.JSON;
      use Yolk.Log;

      J : JSON_Value;

      SC : constant String := AWS.Messages.Status_Code'Image
        (Error_Messages (Err).Status_Code);
   begin
      J := Create_Object;

      if Message /= "" then
         Append (Source   => Error_Messages (Err).Description,
                 New_Item => " - " & Message);
      end if;

      Trace (Error, Error_Type'Image (Err)
             & " - "
             & To_String (Error_Messages (Err).Status)
             & " - "
             & To_String (Error_Messages (Err).Description)
             & " - "
             & SC);

      J := Create_Object;

      J.Set_Field (Field_Name => "description",
                   Field      => Error_Messages (Err).Description);
      J.Set_Field (Field_Name => "status",
                   Field      => Error_Messages (Err).Status);

      Error_Messages (Err).JSON := To_JSON_String (J.Write);

      return Error_Messages (Err);
   end Log_Exception;

   ---------------------
   --  Log_Exception  --
   ---------------------

   procedure Log_Exception
     (Event   : in Ada.Exceptions.Exception_Occurrence;
      Message : in String)
   is
      use Ada.Exceptions;
      use Yolk.Log;

      E_Name : constant String := Exception_Name (Event);
      E_Msg  : constant String := Exception_Message (Event);
   begin
      Trace (Error, E_Name
             & " - "
             & E_Msg
             & " - "
             & Message);
   end Log_Exception;

   -------------------
   --  Status_Code  --
   -------------------

   function Status_Code
     (Err : in Error_Record)
      return AWS.Messages.Status_Code
   is
   begin
      return Err.Status_Code;
   end Status_Code;

end Errors;
