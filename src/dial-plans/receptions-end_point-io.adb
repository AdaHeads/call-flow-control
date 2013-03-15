-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2013-, AdaHeads K/S                    --
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

with DOM.Core.Nodes,
     DOM.Support;

with Receptions.End_Points.Busy_Signal,
     Receptions.End_Points.Hang_Up,
     Receptions.End_Points.Interactive_Voice_Response,
     Receptions.End_Points.Queue,
     Receptions.End_Points.Redirect,
     Receptions.End_Points.Voice_Mail;

package body Receptions.End_Point.IO is
   function Load (From : in DOM.Core.Node) return Class is
      package Busy_Signal renames Receptions.End_Points.Busy_Signal;
      package Hang_Up renames Receptions.End_Points.Hang_Up;
      package Interactive_Voice_Response renames
                              Receptions.End_Points.Interactive_Voice_Response;
      package Queue renames Receptions.End_Points.Queue;
      package Redirect renames Receptions.End_Points.Redirect;
      package Voice_Mail renames Receptions.End_Points.Voice_Mail;

      use DOM.Core, DOM.Core.Nodes, DOM.Support;

      Title  : constant String := Attribute (From, "title");
      Action : DOM.Core.Node;
   begin
      DOM.Support.Check (Element => From,
                         Name    => XML_Element_Name);

      Action := DOM.Core.Nodes.First_Child (From);

      loop
         exit when Action = null;
         exit when Node_Type (Action) = Element_Node;
         DOM.Support.Next (Element => Action);
      end loop;

      if Node_Name (Action) = Hang_Up.XML_Element_Name then
         return Hang_Up.Create (Title => Title);
      elsif Node_Name (Action) = Queue.XML_Element_Name then
         declare
            Q : constant Queue.Instance :=
                  Queue.Create
                    (Title => Title,
                     ID    => Attribute (Element => Action,
                                         Name    => "id"));
         begin
            return Q;
         end;
      elsif Node_Name (Action) = Redirect.XML_Element_Name then
         declare
            R : constant Redirect.Instance :=
                  Redirect.Create
                    (Title => Title,
                     To    => Attribute
                                (Element => Action,
                                 Name    => "to"));
         begin
            return R;
         end;
      elsif Node_Name (Action) = Interactive_Voice_Response.XML_Element_Name
      then
         return Interactive_Voice_Response.Create (Title => Title);
      elsif Node_Name (Action) = Voice_Mail.XML_Element_Name then
         declare
            V : constant Voice_Mail.Instance :=
                  Voice_Mail.Create
                    (Title   => Title,
                     Play    => Attribute
                                  (Element => Action,
                                   Name    => "play"),
                     Send_To => Attribute
                                  (Element => Action,
                                   Name    => "send-to"));
         begin
            return V;
         end;
      elsif Node_Name (Action) = Busy_Signal.XML_Element_Name then
         return Busy_Signal.Create (Title => Title);
      else
         raise Constraint_Error
           with "<" & Receptions.End_Point.XML_Element_Name &
                "> element contains illegal element <" & Node_Name (Action) &
                ">.";
      end if;
   end Load;
end Receptions.End_Point.IO;
