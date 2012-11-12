-------------------------------------------------------------------------------
--                                                                           --
--                                  Alice                                    --
--                                                                           --
--                           Model.Organizations                             --
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
with View.Organization;

package body Model.Organizations is

   use GNATCOLL.SQL;
   use GNATCOLL.SQL.Exec;

   package DB renames Database;

   procedure Fetch_Organization_Object is new Storage.Process_Query
     (Database_Cursor   => Cursor,
      Element           => Organization_Object,
      Cursor_To_Element => Organization_Element);

   ---------------------------------------------------------------------------
   --  Prepared statement for fetching an organization entity with all its  --
   --  associated attributes.                                               --
   ---------------------------------------------------------------------------

   Organization_Query : constant SQL_Query
     := SQL_Select (Fields =>
                      DB.Organization.Full_Name &   --  0
                      DB.Organization.Identifier &  --  1
                      DB.Organization.Json &        --  2
                      DB.Organization.Id,           --  3
                    From   => DB.Organization,
                    Where  =>
                      DB.Organization.Id = Integer_Param (1));

   Prepared_Organization_Query : constant Prepared_Statement
     := Prepare (Query         => Organization_Query,
                 Auto_Complete => True,
                 On_Server     => True,
                 Name          => "organization");

   -----------------
   --  Full_Name  --
   -----------------

   function Full_Name
     (Organization : in Organization_Object)
      return String
   is
   begin
      return To_String (Organization.Full_Name);
   end Full_Name;

   -----------
   --  Get  --
   -----------

   procedure Get
     (O_Id    : in Organization_Identifier;
      Process : not null access
        procedure (Element : in Organization_Object'Class))
   is
      Parameters : constant SQL_Parameters := (1 => +Integer (O_Id));
   begin
      Fetch_Organization_Object
        (Process_Element    => Process,
         Prepared_Statement => Prepared_Organization_Query,
         Query_Parameters   => Parameters);
      null;
   end Get;

   -----------
   --  Get  --
   -----------

   function Get
     (O_Id : in Organization_Identifier)
      return Organization_Object
   is
      procedure Get_Element
        (Organization : in Organization_Object'Class);

      O : Organization_Object := Null_Organization_Object;

      -------------------
      --  Get_Element  --
      -------------------

      procedure Get_Element
        (Organization : in Organization_Object'Class)
      is
      begin
         O := Organization_Object (Organization);
      end Get_Element;
   begin
      Get (O_Id, Get_Element'Access);
      return O;
   end Get;

   ------------------
   --  Identifier  --
   ------------------

   function Identifier
     (Organization : in Organization_Object)
      return String
   is
   begin
      return To_String (Organization.Identifier);
   end Identifier;

   ------------
   --  JSON  --
   ------------

   function JSON
     (Organization : in Organization_Object)
      return GNATCOLL.JSON.JSON_Value
   is
   begin
      return Organization.JSON;
   end JSON;

   ----------------------------
   --  Organization_Element  --
   ----------------------------

   function Organization_Element
     (C : in out Cursor)
      return Organization_Object'Class
   is
      use Common;

      O : Organization_Object;
   begin
      O := Organization_Object'
        (Full_Name  => U (C.Value (0)),
         Identifier => U (C.Value (1)),
         JSON       => C.Json_Object_Value (2),
         O_Id       => Organization_Identifier (C.Integer_Value (3)));

      return O;
   end Organization_Element;

   -----------------------
   --  Organization_Id  --
   -----------------------

   function Organization_Id
     (Organization : in Organization_Object)
      return Organization_Identifier
   is
   begin
      return Organization.O_Id;
   end Organization_Id;

   ---------------
   --  To_JSON  --
   ---------------

   function To_JSON
     (Organization : in Organization_Object)
      return Common.JSON_String
   is
   begin
      return View.Organization.To_JSON (Organization);
   end To_JSON;

end Model.Organizations;
