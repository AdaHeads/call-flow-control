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

with Ada.Strings.Fixed;

package body AMI.Peer_ID is

   function "<" (Left  : in Peer_ID_Type;
                 Right : in Peer_ID_Type) return Boolean is
   begin
      return Left.Kind < Right.Kind and then
        Left.Peername < Right.Peername;
   end "<";

   function "=" (Left  : in Peer_ID_Type;
                 Right : in Peer_ID_Type) return Boolean is
   begin
      return Left.Kind = Right.Kind and Left.Peername = Right.Peername;
   end "=";

   function Create (Item : in String) return Peer_ID_Type is
      Kind_Offset    : constant Natural := Ada.Strings.Fixed.Index
        (Source => Item, Pattern => "/");
   begin
      if Kind_Offset > 2 then
         return
           Create (Channel_Kind =>
                     Item (Item'First .. Item'First + Kind_Offset - 2),
                   Peername     =>
                     Item (Item'First + Kind_Offset .. Item'Last));
      end if;
      raise Constraint_Error;
   exception
      when Constraint_Error =>
         raise Invalid_ID with "Invalid Peer ID: " & Item;
   end Create;

   function Create (Channel_Kind : in String;
                    Peername     : in String) return Peer_ID_Type is
   begin
      return (Kind     => Channel_Type'Value (Channel_Kind),
              Peername => To_Unbounded_String (Peername));
   end Create;

   function To_String (Peer_ID : in Peer_ID_Type) return String is
   begin
      if Peer_ID = Null_Peer_ID then
         return "<null>";
      else
         return Peer_ID.Kind'Img & "/" & To_String (Peer_ID.Peername);
      end if;
   end To_String;

end AMI.Peer_ID;
