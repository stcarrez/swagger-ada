-----------------------------------------------------------------------
--  swagger-server -- Rest server support
--  Copyright (C) 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Util.Beans.Objects.Readers;
with Util.Serialize.IO.JSON;
with ASF.Streams;
package body Swagger.Servers is

   --  ------------------------------
   --  Get a request parameter defined in the URI path.
   --  ------------------------------
   procedure Get_Path_Parameter (Req   : in Request'Class;
                                 Pos   : in Positive;
                                 Value : out UString) is
   begin
      Value := To_UString (Req.Get_Path_Parameter (Pos));
   end Get_Path_Parameter;

   --  ------------------------------
   --  Get a request parameter defined in the URI path.
   --  ------------------------------
   procedure Get_Path_Parameter (Req   : in Request'Class;
                                 Pos   : in Positive;
                                 Value : out Long) is
      V : constant String := Req.Get_Path_Parameter (Pos);
   begin
      Value := Long'Value (V);
   end Get_Path_Parameter;

   --  ------------------------------
   --  Get a request parameter from the query string.
   --  ------------------------------
   procedure Get_Query_Parameter (Req   : in Request'Class;
                                  Name  : in String;
                                  Value : out UString) is
   begin
      Value := To_UString (Req.Get_Parameter (Name));
   end Get_Query_Parameter;

   procedure Get_Query_Parameter (Req   : in Request'Class;
                                  Name  : in String;
                                  Value : out Nullable_UString) is
      V : constant String := Req.Get_Parameter (Name);
   begin
      Value.Value := To_UString (V);
      Value.Is_Null := V'Length = 0;
   end Get_Query_Parameter;

   --  Get a request parameter from the query string.
   procedure Get_Query_Parameter (Req   : in Request'Class;
                                  Name  : in String;
                                  Value : out UString_Vectors.Vector) is
   begin
      Value.Append (Req.Get_Parameter (Name));
   end Get_Query_Parameter;

   --  ------------------------------
   --  Get a request parameter passed in the form.
   --  ------------------------------
   procedure Get_Parameter (Req   : in Request'Class;
                            Name  : in String;
                            Value : out Long) is
      V : constant String := Req.Get_Parameter (Name);
   begin
      Value := Long'Value (V);
   end Get_Parameter;

   --  ------------------------------
   --  Get a request parameter passed in the form.
   --  ------------------------------
   procedure Get_Parameter (Req   : in Request'Class;
                            Name  : in String;
                            Value : out Integer) is
      V : constant String := Req.Get_Parameter (Name);
   begin
      Value := Integer'Value (V);
   end Get_Parameter;

   --  ------------------------------
   --  Get a request parameter passed in the form.
   --  ------------------------------
   procedure Get_Parameter (Req   : in Request'Class;
                            Name  : in String;
                            Value : out UString) is
   begin
      Value := To_UString (Req.Get_Parameter (Name));
   end Get_Parameter;

   procedure Get_Parameter (Req   : in Request'Class;
                            Name  : in String;
                            Value : out Nullable_UString) is
      Param : constant String := Req.Get_Parameter (Name);
   begin
      Value.Value := To_UString (Param);
      Value.Is_Null := Param'Length = 0;
   end Get_Parameter;

   --  ------------------------------
   --  Get a request parameter passed in the form.
   --  ------------------------------
   procedure Get_Parameter (Req   : in Request'Class;
                            Name  : in String;
                            Value : out Boolean) is
      V : constant String := Req.Get_Parameter (Name);
   begin
      Value := Boolean'Value (V);
   end Get_Parameter;

   --  ------------------------------
   --  Read the request body and get a value object tree.
   --  ------------------------------
   procedure Read (Req   : in Request'Class;
                   Value : out Value_Type) is
      use type ASF.Streams.Input_Stream_Access;

      Stream : constant ASF.Streams.Input_Stream_Access := Req.Get_Input_Stream;
      Parser : Util.Serialize.IO.JSON.Parser;
      Mapper : Util.Beans.Objects.Readers.Reader;
   begin
      if Stream = null then
         Value := Util.Beans.Objects.Null_Object;
      else
         Parser.Parse (Stream.all, Mapper);
         Value := Mapper.Get_Root;
      end if;
   end Read;

   procedure Initialize (Context : in out Context_Type;
                         Req     : in out Request'Class;
                         Reply   : in out Response'Class) is
   begin
      Context.Req := Req'Unchecked_Access;
      Context.Reply := Reply'Unchecked_Access;
   end Initialize;

   --  ------------------------------
   -- Set the response error code with a message to return.
   --  ------------------------------
   procedure Set_Error (Context : in out Context_Type;
                        Code    : in Natural;
                        Message : in String) is
   begin
      Context.Reply.Set_Status (Code);
   end Set_Error;

   --  ------------------------------
   --  Returns True if the client doing the request has the given permission.
   --  ------------------------------
   function Has_Permission (Context    : in Context_Type;
                            Permission : in Security.Permissions.Permission_Index)
                            return Boolean is
   begin
      return True;
   end Has_Permission;

end Swagger.Servers;
