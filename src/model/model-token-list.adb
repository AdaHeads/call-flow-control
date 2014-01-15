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

package body Model.Token.List is

   Singleton : Instance;
   --  Singleton instance.

   ---------------------
   --  Get_Singleton  --
   ---------------------

   function Get_Singleton return Instance is
   begin
      return Singleton;
   end Get_Singleton;

   ---------------
   --  Look_Up  --
   ----------------

   function Look_Up (Object     : Instance;
                     User_Token : Token.Instance) return User.Identities is
   begin
      if not Object.Tokens.Contains (User_Token) then
         return User.Null_Identity;
      end if;

      return Object.Tokens.Element (Key => User_Token);
   end Look_Up;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON (Object : in Instance) return JSON_Value is
      use Token_User_Storage;

      JSON_List : JSON_Array;
      Root      : constant JSON_Value := Create_Object;
   begin
      for Cursor in Object.Tokens.Iterate loop
         declare
            Token_Node    : constant JSON_Value := Create_Object;
         begin
            Token_Node.Set_Field (To_String (Key (Cursor)), Element (Cursor));

            Append (JSON_List, Token_Node);
         end;
      end loop;
      Root.Set_Field ("tokens", JSON_List);
      return Root;
   end To_JSON;
end Model.Token.List;
