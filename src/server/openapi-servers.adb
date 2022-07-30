-----------------------------------------------------------------------
--  openapi-server -- Rest server support
--  Copyright (C) 2017, 2020, 2022 Stephane Carrez
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
with Util.Http.Headers;
with Util.Beans.Objects.Readers;
with Util.Serialize.IO.JSON;
with Util.Serialize.IO.XML;
with Util.Serialize.IO.Form;
with Security;
with Servlet.Streams;
with Servlet.Responses;
package body OpenAPI.Servers is

   use type Servlet.Streams.Input_Stream_Access;

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
   --  Get a request parameter defined in the URI path.
   --  ------------------------------
   function Get_Path_Parameter (Req   : in Request'Class;
                                Pos   : in Positive) return String is
   begin
      return Req.Get_Path_Parameter (Pos);
   end Get_Path_Parameter;

   --  ------------------------------
   --  Get a request parameter from the query string.
   --  ------------------------------
   function Get_Query_Parameter (Req   : in Request'Class;
                                 Name  : in String) return String is
   begin
      return Req.Get_Parameter (Name);
   end Get_Query_Parameter;

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

   --  ------------------------------
   --  Get a request parameter from the query as boolean.
   --  ------------------------------
   procedure Get_Query_Parameter (Req   : in Request'Class;
                                  Name  : in String;
                                  Value : out Boolean) is
   begin
      Value := Boolean'Value (Req.Get_Parameter (Name));
   end Get_Query_Parameter;

   procedure Get_Query_Parameter (Req   : in Request'Class;
                                  Name  : in String;
                                  Value : out Nullable_Boolean) is
      V : constant String := Req.Get_Parameter (Name);
   begin
      Value.Is_Null := V'Length = 0;
      if Value.Is_Null then
         Value.Value := False;
      else
         Value.Value := Boolean'Value (V);
      end if;
   end Get_Query_Parameter;

   --  Get a request parameter from the query string.
   procedure Get_Query_Parameter (Req   : in Request'Class;
                                  Name  : in String;
                                  Value : out UString_Vectors.Vector) is
   begin
      Value.Append (Req.Get_Parameter (Name));
   end Get_Query_Parameter;

   procedure Get_Query_Parameter (Req   : in Request'Class;
                                  Name  : in String;
                                  Value : out Nullable_UString_Vectors.Vector) is
      Param : constant String := Req.Get_Parameter (Name);
   begin
      if Param'Length > 0 then
         Value.Append ((Is_Null => False, Value => To_UString (Param)));
      end if;
   end Get_Query_Parameter;

   --  ------------------------------
   --  Read the request body and get a value object tree.
   --  ------------------------------
   procedure Read (Req      : in Request'Class;
                   Consumes : in Mime_List;
                   Value    : out Value_Type) is
      Kind   : constant String := Req.Get_Header (Util.Http.Headers.Content_Type);
      Stream : constant Servlet.Streams.Input_Stream_Access := Req.Get_Input_Stream;
      Mapper : Util.Beans.Objects.Readers.Reader;
   begin
      for Media of Consumes loop
         if Media.all = Kind then
            if Media.all = Util.Http.Mimes.Json then
               declare
                  Parser : Util.Serialize.IO.JSON.Parser;
               begin
                  Parser.Parse (Stream.all, Mapper);
               end;
            elsif Media.all = Util.Http.Mimes.Xml then
               declare
                  Parser : Util.Serialize.IO.XML.Parser;
               begin
                  Parser.Parse (Stream.all, Mapper);
               end;
            elsif Media.all = Util.Http.Mimes.Form then
               declare
                  Parser : Util.Serialize.IO.Form.Parser;
               begin
                  Parser.Parse (Stream.all, Mapper);
               end;
            end if;
            Value := Mapper.Get_Root;
            return;
         end if;
      end loop;
      Value := Util.Beans.Objects.Null_Object;
   end Read;

   procedure Read (Context : in out Context_Type) is
      Stream : constant Servlet.Streams.Input_Stream_Access := Context.Req.Get_Input_Stream;
      Parser : Util.Serialize.IO.Form.Parser;
      Mapper : Util.Beans.Objects.Readers.Reader;
   begin
      if Stream = null then
         Context.Params := Util.Beans.Objects.Null_Object;
      else
         Parser.Parse (Stream.all, Mapper);
         Context.Params := Mapper.Get_Root;
      end if;
      Context.Use_Map := True;
   end Read;

   procedure Initialize (Context : in out Context_Type;
                         Req     : in out Request'Class;
                         Reply   : in out Response'Class;
                         Stream  : in out Output_Stream'Class) is
   begin
      Context.Req := Req'Unchecked_Access;
      Context.Reply := Reply'Unchecked_Access;
      Context.Stream := Stream'Unchecked_Access;
      if Req.Get_Method = "PUT"
        and then Req.Get_Content_Type = "application/x-www-form-urlencoded"
      then
         Context.Read;
      end if;
   end Initialize;

   function Get_Parameter (Req : in out Context_Type;
                           Name : in String) return String is
   begin
      if Req.Use_Map then
         return Util.Beans.Objects.To_String (Util.Beans.Objects.Get_Value (Req.Params, Name));
      else
         return Req.Req.Get_Parameter (Name);
      end if;
   end Get_Parameter;

   --  ------------------------------
   --  Get a request parameter passed in the form.
   --  ------------------------------
   procedure Get_Parameter (Req   : in out Context_Type;
                            Name  : in String;
                            Value : out Long) is
      V : constant String := Req.Get_Parameter (Name);
   begin
      Value := Long'Value (V);
   end Get_Parameter;

   --  ------------------------------
   --  Get a request parameter passed in the form.
   --  ------------------------------
   procedure Get_Parameter (Req   : in out Context_Type;
                            Name  : in String;
                            Value : out Integer) is
      V : constant String := Req.Get_Parameter (Name);
   begin
      Value := Integer'Value (V);
   end Get_Parameter;

   --  ------------------------------
   --  Get a request parameter passed in the form.
   --  ------------------------------
   procedure Get_Parameter (Req   : in out Context_Type;
                            Name  : in String;
                            Value : out UString) is
   begin
      Value := To_UString (Req.Get_Parameter (Name));
   end Get_Parameter;

   procedure Get_Parameter (Req   : in out Context_Type;
                            Name  : in String;
                            Value : out Nullable_UString) is
   begin
      if Req.Use_Map then
         declare
            Item : constant Util.Beans.Objects.Object
              := Util.Beans.Objects.Get_Value (Req.Params, Name);
         begin
            Value.Is_Null := Util.Beans.Objects.Is_Null (Item);
            Value.Value := Util.Beans.Objects.To_Unbounded_String (Item);
         end;
      else
         declare
            Param : constant String := Req.Get_Parameter (Name);
         begin
            Value.Value := To_UString (Param);
            Value.Is_Null := Param'Length = 0;
         end;
      end if;
   end Get_Parameter;

   --  ------------------------------
   --  Get a request parameter passed in the form.
   --  ------------------------------
   procedure Get_Parameter (Req   : in out Context_Type;
                            Name  : in String;
                            Value : out Boolean) is
      V : constant String := Req.Get_Parameter (Name);
   begin
      Value := Boolean'Value (V);
   end Get_Parameter;

   --  ------------------------------
   --  Set the response error code with a message to return.
   --  ------------------------------
   procedure Set_Error (Context : in out Context_Type;
                        Code    : in Natural;
                        Message : in String) is
   begin
      Context.Reply.Set_Status (Code);
      Context.Stream.Start_Document;
      Context.Stream.Start_Entity ("");
      Context.Stream.Write_Attribute ("code", Code);
      Context.Stream.Write_Attribute ("message", Message);
      Context.Stream.End_Entity ("");
      Context.Stream.End_Document;
   end Set_Error;

   --  ------------------------------
   --  Set the HTTP status in the response.
   --  ------------------------------
   procedure Set_Status (Context : in out Context_Type;
                         Code    : in Natural) is
   begin
      Context.Reply.Set_Status (Code);
   end Set_Status;

   --  ------------------------------
   --  Set the response description in the X-OpenAPI-Message header when enabled.
   --  ------------------------------
   procedure Set_Description (Context : in out Context_Type;
                              Message : in String) is
   begin
      Context.Reply.Set_Header ("X-OpenAPI-Message", Message);
   end Set_Description;

   --  ------------------------------
   --  Get the HTTP status that will be sent in the response.
   --  ------------------------------
   function Get_Status (Context : in Context_Type) return Natural is
   begin
      return Context.Reply.Get_Status;
   end Get_Status;

   --  ------------------------------
   --  Send a Location: header in the response.
   --  ------------------------------
   procedure Set_Location (Context : in out Context_Type;
                           URL     : in String) is
   begin
      Context.Reply.Add_Header (Name  => "Location", Value => URL);
   end Set_Location;

   --  ------------------------------
   --  Returns True if the API request is authenticated.
   --  ------------------------------
   function Is_Authenticated (Context : in Context_Type) return Boolean is
      use type Security.Principal_Access;

      User : constant Security.Principal_Access := Context.Req.Get_User_Principal;
   begin
      return User /= null;
   end Is_Authenticated;

   --  ------------------------------
   --  Returns True if the client doing the request has the given permission.
   --  ------------------------------
   function Has_Permission (Context    : in Context_Type;
                            Permission : in Security.Permissions.Permission_Index)
                           return Boolean is
      pragma Unreferenced (Permission);

      use type Security.Principal_Access;

      User : constant Security.Principal_Access := Context.Req.Get_User_Principal;
   begin
      if User = null then
         return False;
      end if;
      return True;
   end Has_Permission;

end OpenAPI.Servers;
