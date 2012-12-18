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

with View.Call;

package body Model.Call is

   --------------------------
   -- Overloaded operators --
   --------------------------

   function "=" (Left  : in Call_Type;
                 Right : in Call_Type) return Boolean is
   begin
      return (Left.ID.Timestamp = Right.ID.Timestamp) and
                (Left.ID.Sequence = Right.ID.Sequence);
   end  "=";

   -------------
   -- To_JSON --
   -------------

   function To_JSON (Call : in Call_Type) return JSON_Value is
   begin
      return View.Call.To_JSON (Call);
   end To_JSON;

   -------------
   -- To_JSON --
   -------------

   function To_String (Call : in Call_Type) return String is
      Response : Unbounded_String;
   begin
      Append (Response, "ID => " & Call.ID.To_String);
      Append (Response, ", Channel => " & Call.Channel.Image);
      Append (Response, ", Queue => "    & To_String (Call.Queue));
      Append (Response, ", State => "    & Call.State'Img);
      return To_String (Response);
   end To_String;

end Model.Call;
