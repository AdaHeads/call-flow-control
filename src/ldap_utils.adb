-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                LDAP_Utils                                 --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body LDAP_Utils is

   function To_JSON
     (Directory    : in AWS.LDAP.Client.Directory;
      Response_Set : in AWS.LDAP.Client.LDAP_Message)
      return JSON_Value
   is
      use Ada.Strings.Unbounded;
      use GNATCOLL.JSON;

      Objects_JSON : constant JSON_Value  := Create_Object;
      Objects_Arr  : JSON_Array;
      Message      : AWS.LDAP.Client.LDAP_Message;
      BER          : aliased AWS.LDAP.Client.BER_Element;
   begin
      Message := AWS.LDAP.Client.First_Entry (Directory, Response_Set);

      while Message /= AWS.LDAP.Client.Null_LDAP_Message loop
         declare
            Attrs      : Unbounded_String;
            Attrs_JSON : JSON_Value := Create_Object;
         begin
            Attrs := To_Unbounded_String
              (AWS.LDAP.Client.First_Attribute (Directory, Message,
               BER'Unchecked_Access));
            loop
               declare
                  RS : AWS.LDAP.Client.String_Set := AWS.LDAP.Client.Get_Values
                    (Directory, Message, To_String (Attrs));
               begin
                  -- if there is more than one attribute, convert to an array
                  if RS'Length > 1 then
                     declare
                        Values : JSON_Array;
                     begin
                        for K in RS'Range loop
                           Append (Arr => Values,
                                   Val => Create (To_String (RS (K))));
                        end loop;
                        Attrs_JSON.Set_Field (Field_Name => To_String (Attrs),
                                              Field      =>  Values);
                     end;
                  else
                     -- Otherwise, just add as a field
                     Attrs_JSON.Set_Field (Field_Name => To_String (Attrs),
                                           Field      =>  To_String (RS (1)));

                  end if;
               end;
               Objects_JSON.Set_Field
                 (AWS.LDAP.Client.Get_DN (Directory, Message),
                  Attrs_JSON);
               -- Next element
               Attrs := To_Unbounded_String
                 (AWS.LDAP.Client.Next_Attribute (Directory, Message, BER));

               --  Exit when no more attributes
               exit when Attrs = Null_Unbounded_String;
            end loop;

            AWS.LDAP.Client.Free (BER);
         end;

         --  Get the next entry
         Message := AWS.LDAP.Client.Next_Entry (Directory, Message);
      end loop;

      --  Free memory
      AWS.LDAP.Client.Free (Message);

      return Objects_JSON;
   end To_JSON;

end LDAP_Utils;
