-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                              Model.Contact                                --
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

with Database;
with Storage;
with View.Contact;

package body Model.Contact is

   use GNATCOLL.SQL;
   use GNATCOLL.SQL.Exec;

   package DB renames Database;

   procedure Fetch_Contact_Object is new Storage.Process_Query
     (Database_Cursor   => Cursor,
      Element           => Contact_Object,
      Cursor_To_Element => Contact_Element);

   --------------------------------------------------------
   --  Prepared statement for fetching a contact entity  --
   --------------------------------------------------------

   Contact_Query_Left_Join : constant SQL_Left_Join_Table
     :=  Left_Join (Full    => DB.Contact,
                    Partial => DB.Contact_Attributes,
                    On      =>
                      DB.Contact_Attributes.FK (DB.Contact));

   Contact_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Contact.Id &                         --  0
                      DB.Contact.Full_Name &                  --  1
                      DB.Contact.Is_Human &                   --  2
                      DB.Contact_Attributes.Json &            --  3
                      DB.Contact_Attributes.Contact_Id &      --  4
                      DB.Contact_Attributes.Organization_Id,  --  5
                    From   => Contact_Query_Left_Join,
                    Where  => DB.Contact.Id = Integer_Param (1));

   Prepared_Contact_Query : constant Prepared_Statement
     := Prepare (Query         => Contact_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "contact");

   -----------------------
   --  Contact_Element  --
   -----------------------

   function Contact_Element
     (C : in out Cursor)
      return Contact_Object'Class
   is
      use Common;

      CO : Contact_Object;
   begin
      CO := Contact_Object'
        (Attr_List   => Attributes_List.Empty_List,
         C_Id        => Contact_Id (C.Integer_Value (0, Default => 0)),
         Full_Name   => U (C.Value (1)),
         Is_Human    => C.Boolean_Value (2));

      while C.Has_Row loop
         if not C.Is_Null (3) then
            CO.Attr_List.Append (Model.Contact_Attributes.Create
              (JSON => C.Json_Object_Value (3),
               C_Id => Contact_Id (C.Integer_Value (4, Default => 0)),
               O_Id => Organization_Id (C.Integer_Value (5, Default => 0))));
         end if;

         C.Next;
      end loop;

      return CO;
   end Contact_Element;

   -------------
   --  Equal  --
   -------------

   function Equal
     (Left, Right : in Model.Contact_Attributes.Contact_Attributes_Object)
      return Boolean
   is
      use Model.Contact_Attributes;
   begin
      return Left = Right;
   end Equal;

   -----------
   --  Get  --
   -----------

   procedure Get
     (Id      : in Contact_Id;
      Process : not null access
        procedure (Element : in Contact_Object'Class))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (Id));
   begin
      Fetch_Contact_Object (Process_Element    => Process,
                            Prepared_Statement => Prepared_Contact_Query,
                            Query_Parameters   => Parameters);
   end Get;

   -----------
   --  Get  --
   -----------

   function Get
     (Id : in Contact_Id)
      return Contact_Object
   is
      procedure Get_Element
        (Contact : in Contact_Object'Class);

      C : Contact_Object := Null_Contact_Object;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Contact : in Contact_Object'Class)
      is
      begin
         C := Contact_Object (Contact);
      end Get_Element;
   begin
      Get (Id, Get_Element'Access);
      return C;
   end Get;

   ----------------------
   --  Get_Attributes  --
   ----------------------

   function Get_Attributes
     (Contact : in Contact_Object)
      return Attributes_List.List
   is
   begin
      return Contact.Attr_List;
   end Get_Attributes;

   ----------------------
   --  Get_Contact_Id  --
   ----------------------

   function Get_Contact_Id
     (Contact : in Contact_Object)
      return Contact_Id
   is
   begin
      return Contact.C_Id;
   end Get_Contact_Id;

   ---------------------
   --  Get_Full_Name  --
   ---------------------

   function Get_Full_Name
     (Contact : in Contact_Object)
      return String
   is
   begin
      return To_String (Contact.Full_Name);
   end Get_Full_Name;

   --------------------
   --  Get_Is_Human  --
   --------------------

   function Get_Is_Human
     (Contact : in Contact_Object)
      return Boolean
   is
   begin
      return Contact.Is_Human;
   end Get_Is_Human;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Contact : in Contact_Object)
      return Common.JSON_String
   is
   begin
      return View.Contact.To_JSON (Contact);
   end To_JSON;

end Model.Contact;
