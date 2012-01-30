-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                                LDAP.Read                                  --
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

with AWS.LDAP.Thin;
with AWS.Utils;
with GNATCOLL.JSON;
with Interfaces.C;
with Yolk.Utilities;

package body LDAP.Read is

   function To_JSON
     (Dir     : in Directory;
      Message : in LDAP_Message)
      return GNATCOLL.JSON.JSON_Value;
   --  Convert a LDAP message to a JSON object.

   --------------
   --  Search  --
   --------------

   function Search
     (Base_Prefix : in String := "";
      Filter      : in String;
      Scope       : in Scope_Type    := LDAP_Scope_Default;
      Attrs       : in Attribute_Set := Null_Set;
      Attrs_Only  : in Boolean       := False)
      return String
   is
      use GNATCOLL.JSON;
      use Interfaces.C;
      use Yolk.Utilities;

      A_Server : Server;
      LDAP_MSG : LDAP_Message;
      Result   : Unbounded_String := Null_Unbounded_String;
      Valid    : Boolean := False;
   begin
      A_Server := Take_Server;

      JSON_Cache.Read
        (Key      => Base_Prefix &  Get_Base_DN (A_Server) & Filter,
         Is_Valid => Valid,
         Value    => Result);

      if Valid then
         Put_Server (A_Server);
         return TS (Result);
      else
         LDAP_MSG := Search
           (A_Server.LDAP_Dir,
            Base_Prefix & Get_Base_DN (A_Server),
            Filter,
            Scope,
            Attrs,
            Attrs_Only);

         Put_Server (A_Server);

         declare
            JSON_String : constant String :=
                            Write (To_JSON (A_Server.LDAP_Dir, LDAP_MSG));
         begin
            JSON_Cache.Write
              (Key   => Base_Prefix &  Get_Base_DN (A_Server) & Filter,
               Value => TUS (JSON_String));

            return JSON_String;
         end;
      end if;

   exception
      when E : LDAP_Error =>
         case Get_Error (E) is
            when -1 =>
               --  Cannot contact LDAP server. Lets mark the server dead and
               --  return an error JSON string.
               Error_Handler
                 (E,
                  "(Search) Exception raised for host " &
                  TS (A_Server.Host) &
                  ":" & AWS.Utils.Image (A_Server.Port) &
                  " with base_dn " & Base_Prefix & Get_Base_DN (A_Server) &
                  " and filter " & Filter);

               Put_Server (A_Server, False);

               return Search (Base_Prefix,
                              Filter,
                              Scope,
                              Attrs,
                              Attrs_Only);
            when AWS.LDAP.Thin.LDAP_NO_SUCH_OBJECT =>
               Put_Server (A_Server);
               return "{}";
            when others =>
               Put_Server (A_Server);

               return Error_Handler
                 (E,
                  "(Search) Exception raised for host " &
                  TS (A_Server.Host) &
                  ":" & AWS.Utils.Image (A_Server.Port) &
                  " with base_dn " & Base_Prefix & Get_Base_DN (A_Server) &
                  " and filter " & Filter);
         end case;
      when Event : others =>
         return Error_Handler (Event,
                               "(Search)" &
                               Base_Prefix &
                               Get_Base_DN (A_Server) &
                               Filter);
   end Search;

   ----------------------
   --  Search_Company  --
   ----------------------

   function Search_Company
     (o : in String)
      return String
   is
      Filter : constant String :=
                 "(&(objectClass=organization)(o=" & o & "))";
   begin
      return Search (Filter      => Filter,
                     Scope       => LDAP_Scope_Subtree,
                     Attrs       => Attributes ("*"));
   end Search_Company;

   ----------------------
   --  Search_Person  --
   ----------------------

   function Search_Person
     (o  : in String;
      cn : in String)
      return String
   is
      Prefix : constant String := "o=" & o & ",";
      Filter : constant String := "(&(objectclass=person)(cn=" & cn & "))";
   begin
      return Search (Base_Prefix => Prefix,
                     Filter      => Filter,
                     Scope       => LDAP_Scope_Subtree,
                     Attrs       => Attributes ("*"));
   end Search_Person;

   ----------------------
   --  Search_Persons  --
   ----------------------

   function Search_Persons
     (o : in String)
      return String
   is
      Prefix : constant String := "o=" & o & ",";
      Filter : constant String := "(objectClass=person)";
   begin
      return Search (Base_Prefix => Prefix,
                     Filter      => Filter,
                     Scope       => LDAP_Scope_Subtree,
                     Attrs       => Attributes ("*"));
   end Search_Persons;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Dir     : in Directory;
      Message : in LDAP_Message)
      return GNATCOLL.JSON.JSON_Value
   is
      use AWS.LDAP.Thin;
      use GNATCOLL.JSON;
      use Yolk.Utilities;

      BER          : aliased BER_Element;
      LDAP_MSG     : LDAP_Message;
      Objects_JSON : constant JSON_Value  := Create_Object;
   begin
      LDAP_MSG := First_Entry (Dir, Message);

      while LDAP_MSG /= Null_LDAP_Message loop
         declare
            Attrs      : Unbounded_String;
            Attrs_JSON : constant JSON_Value := Create_Object;
         begin
            Attrs := TUS
              (First_Attribute (Dir, LDAP_MSG, BER'Unchecked_Access));
            loop
               declare
                  RS : constant String_Set := Get_Values
                    (Dir, LDAP_MSG, TS (Attrs));
               begin
                  --  if there is more than one attribute, convert to an array
                  if RS'Length > 1 then
                     declare
                        Values : JSON_Array;
                     begin
                        for K in RS'Range loop
                           Append (Arr => Values,
                                   Val => Create (TS (RS (K))));
                        end loop;
                        Attrs_JSON.Set_Field (Field_Name => TS (Attrs),
                                              Field      => Values);
                     end;
                  else
                     --  Otherwise, just add as a field
                     Attrs_JSON.Set_Field (Field_Name => TS (Attrs),
                                           Field      => TS (RS (1)));

                  end if;
               end;
               Objects_JSON.Set_Field
                 (Get_DN (Dir, LDAP_MSG), Attrs_JSON);
               --  Next element
               Attrs := TUS (Next_Attribute (Dir, LDAP_MSG, BER));

               --  Exit when no more attributes
               exit when Attrs = Null_Unbounded_String;
            end loop;

            Free (BER);
         end;

         --  Get the next entry
         LDAP_MSG := Next_Entry (Dir, LDAP_MSG);
      end loop;

      --  Free memory
      Free (LDAP_MSG);

      return Objects_JSON;
   end To_JSON;

end LDAP.Read;
