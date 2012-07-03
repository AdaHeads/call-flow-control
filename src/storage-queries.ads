-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                             Storage.Queries                               --
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

with Ada.Strings.Unbounded;
with Common;

package Storage.Queries is

   type Pair_Boolean is
      record
         Name  : Ada.Strings.Unbounded.Unbounded_String;
         Value : Boolean;
      end record;

   type Pair_Natural is
      record
         Name  : Ada.Strings.Unbounded.Unbounded_String;
         Value : Natural;
      end record;

   type Pair_String is
      record
         Name  : Ada.Strings.Unbounded.Unbounded_String;
         Value : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   ---------------------------------------------
   --  Contact Full record, cursor and query  --
   ---------------------------------------------

   type Contact_Full_Cursor is new GNATCOLL.SQL.Exec.Forward_Cursor with
     null record;

   type Contact_Full_Row is
      record
         JSON        : Common.JSON_String;
         Ce_Id       : Pair_Natural;
         Ce_Name     : Pair_String;
         Is_Human    : Pair_Boolean;
         Attr_JSON   : Common.JSON_String;
         Attr_Org_Id : Pair_Natural;
         Attr_Ce_Id  : Pair_Natural;
      end record;

   function Element
     (C : in Contact_Full_Cursor)
      return Contact_Full_Row;

   function Contact_Full_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement;

   ---------------------------------------------
   --  Organization record, cursor and query  --
   ---------------------------------------------

   type Organization_Cursor is new GNATCOLL.SQL.Exec.Forward_Cursor with
     null record;

   type Organization_Row is
      record
         JSON       : Common.JSON_String;
         Org_Id     : Pair_Natural;
         Org_Name   : Pair_String;
         Identifier : Pair_String;
      end record;

   function Element
     (C : in Organization_Cursor)
      return Organization_Row;

   function Organization_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement;

   ---------------------------------------------
   --  Org_Contacts record, cursor and query  --
   ---------------------------------------------

   type Org_Contacts_Cursor is new GNATCOLL.SQL.Exec.Forward_Cursor with
     null record;

   type Org_Contacts_Row is
      record
         JSON     : Common.JSON_String;
         Ce_Id    : Pair_Natural;
         Ce_Name  : Pair_String;
         Is_Human : Pair_Boolean;
      end record;

   function Element
     (C : in Org_Contacts_Cursor)
      return Org_Contacts_Row;

   function Org_Contacts_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement;

   --------------------------------------------------------
   --  Org_Contacts_Attributes record, cursor and query  --
   --------------------------------------------------------

   type Org_Contacts_Attributes_Cursor is new
     GNATCOLL.SQL.Exec.Forward_Cursor with null record;

   type Org_Contacts_Attributes_Row is
      record
         JSON   : Common.JSON_String;
         Ce_Id  : Pair_Natural;
         Org_Id : Pair_Natural;
      end record;

   function Element
     (C : in Org_Contacts_Attributes_Cursor)
      return Org_Contacts_Attributes_Row;

   function Org_Contacts_Attributes_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement;

   --------------------------------------------------
   --  Org_Contacts_Full record, cursor and query  --
   --------------------------------------------------

   type Org_Contacts_Full_Cursor is new GNATCOLL.SQL.Exec.Forward_Cursor with
     null record;

   type Org_Contacts_Full_Row is
      record
         JSON        : Common.JSON_String;
         Ce_Id       : Pair_Natural;
         Ce_Name     : Pair_String;
         Is_Human    : Pair_Boolean;
         Attr_JSON   : Common.JSON_String;
         Attr_Org_Id : Pair_Natural;
         Attr_Ce_Id  : Pair_Natural;
      end record;

   function Element
     (C : in Org_Contacts_Full_Cursor)
      return Org_Contacts_Full_Row;

   function Org_Contacts_Full_Query
     return GNATCOLL.SQL.Exec.Prepared_Statement;

end Storage.Queries;
