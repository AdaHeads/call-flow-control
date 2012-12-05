-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              Model.Call_ID                                --
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

package Model.Call_ID is
--  TODO: Make limited
--  TODO: Associate with channel_id in some spectacular fashion.
   type Call_ID_Type is tagged record
      Timestamp : Integer;
      Sequence  : Integer;
   end record;

   Invalid_ID : exception;

   function Create (Item : in String) return Call_ID_Type;
   --  Constructor.

   function To_String (Call_ID : in Call_ID_Type) return String;

   function "<" (Left  : in Call_ID_Type;
                 Right : in Call_ID_Type) return Boolean;

   function "=" (Left  : in Call_ID_Type;
                 Right : in Call_ID_Type) return Boolean;

   Null_Call_ID : constant Call_ID_Type := (-1, -1);

   function Validate (Item : in String) return Boolean;
end Model.Call_ID;
