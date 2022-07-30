-----------------------------------------------------------------------
--  openapi-clients -- Rest client support
--  Copyright (C) 2017, 2018, 2019, 2020, 2022 Stephane Carrez
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
with Util.Serialize.IO.XML;
with Util.Serialize.IO.Form;
with Util.Strings;
with Util.Log.Loggers;
with Util.Http.Mimes;
with OpenAPI.Streams.Forms;
package body OpenAPI.Clients is

   use Ada.Strings.Unbounded;
   use type Util.Strings.Name_Access;

   Log   : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Swagger.Clients");

   type Form_Output_Stream_Access is access all OpenAPI.Streams.Forms.Output_Stream'Class;
   type Json_Output_Stream_Access is access all Util.Serialize.IO.JSON.Output_Stream'Class;

   function Stream (Req : in Request_Type) return Stream_Accessor is
   begin
      return Stream_Accessor '(Stream => Req.Data, N => 0);
   end Stream;

   --  ------------------------------
   --  Set the path to use for the URI.
   --  ------------------------------
   procedure Set_Path (URI  : in out URI_Type;
                       Path : in String) is
   begin
      URI.URI := To_UString (Path);
   end Set_Path;

   --  ------------------------------
   --  Set the path parameter.
   --  ------------------------------
   procedure Set_Path_Param (URI   : in out URI_Type;
                             Name  : in String;
                             Value : in String) is
      Path  : constant String := To_String (URI.URI);
      Pos   : Natural;
      First : constant Natural := Path'First;
   begin
      loop
         Pos := Util.Strings.Index (Path, '{', First);
         exit when Pos = 0;
         if Path (Pos + 1 .. Pos + 1 + Name'Length - 1) = Name
           and then Path (Pos + 1 + Name'Length) = '}'
         then
            URI.URI := To_UString (Path (Path'First .. Pos - 1));
            Append (URI.URI, Value);
            Append (URI.URI, Path (Pos + 1 + Name'Length + 1 .. Path'Last));
            return;
         end if;
         Pos := Util.Strings.Index (Path, '}', Pos + 1);
         exit when Pos = 0;
      end loop;
   end Set_Path_Param;

   --  ------------------------------
   --  Set the path parameter.
   --  ------------------------------
   procedure Set_Path_Param (URI   : in out URI_Type;
                             Name  : in String;
                             Value : in UString) is
   begin
      URI.Set_Path_Param (Name, To_String (Value));
   end Set_Path_Param;

   --  ------------------------------
   --  Add a query parameter.
   --  ------------------------------
   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in String) is
   begin
      if Length (URI.Query) > 0 then
         Append (URI.Query, "&");
      end if;
      Append (URI.Query, Name);
      Append (URI.Query, "=");
      Append (URI.Query, Value);
   end Add_Param;

   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in Nullable_UString) is
   begin
      if not Value.Is_Null then
         URI.Add_Param (Name, Value.Value);
      end if;
   end Add_Param;

   --  ------------------------------
   --  Add a query parameter.
   --  ------------------------------
   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in Boolean) is
   begin
      if Length (URI.Query) > 0 then
         Append (URI.Query, "&");
      end if;
      Append (URI.Query, Name);
      Append (URI.Query, "=");
      Append (URI.Query, Boolean'Image (Value));
   end Add_Param;

   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in Nullable_Boolean) is
   begin
      if not Value.Is_Null then
         URI.Add_Param (Name, Value.Value);
      end if;
   end Add_Param;

   --  ------------------------------
   --  Add a query parameter.
   --  ------------------------------
   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in UString) is
   begin
      Add_Param (URI, Name, To_String (Value));
   end Add_Param;

   --  Add a query parameter.
   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in UString_Vectors.Vector) is

   begin
      if Value.Is_Empty then
         Add_Param (URI, Name, "");
      else  --  if Value.Length = 1 then
         Add_Param (URI, Name, Value.Element (1));
      end if;
   end Add_Param;

   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in Nullable_UString_Vectors.Vector) is
   begin
      if not Value.Is_Empty then
         Add_Param (URI, Name, Value.Element (1).Value);
      end if;
   end Add_Param;

   --  ------------------------------
   --  Convert the URI into a string.
   --  ------------------------------
   function To_String (URI : in URI_Type) return String is
   begin
      if Length (URI.Query) > 0 then
         return To_String (URI.URI) & "?" & To_String (URI.Query);
      else
         return To_String (URI.URI);
      end if;
   end To_String;

   --  ------------------------------
   --  Set the server base URI to connect to.
   --  ------------------------------
   procedure Set_Server (Client : in out Client_Type;
                         Server : in String) is
   begin
      Client.Server := To_UString (Server);
   end Set_Server;

   procedure Set_Server (Client : in out Client_Type;
                         Server : in UString) is
   begin
      Client.Server := Server;
   end Set_Server;

   --  ------------------------------
   --  Set the credential instance that is responsible for populating the HTTP request
   --  before sending the request.
   --  ------------------------------
   procedure Set_Credentials (Client     : in out Client_Type;
                              Credential : access OpenAPI.Credentials.Credential_Type'Class) is
   begin
      Client.Credential := Credential;
   end Set_Credentials;

   procedure Call (Client    : in out Client_Type;
                   Operation : in Operation_Type;
                   URI       : in URI_Type'Class;
                   Request   : in Request_Type'Class) is
      No_Reply : Value_Type;
   begin
      Client.Call (Operation, URI, Request, No_Reply);
   end Call;

   procedure Call (Client    : in out Client_Type;
                   Operation : in Operation_Type;
                   URI       : in URI_Type'Class) is
      No_Reply : Value_Type;
   begin
      Client.Call (Operation, URI, No_Reply);
   end Call;

   procedure Call (Client    : in out Client_Type;
                   Operation : in Operation_Type;
                   URI       : in URI_Type'Class;
                   Reply     : out Value_Type) is
      Path     : constant String := To_String (Client.Server) & To_String (URI);
   begin
      if Client.Credential /= null then
         Client.Credential.Set_Credentials (Client);
      end if;

      case Operation is
         when HEAD =>
            Client.Head (Path, Client.Response);

         when GET =>
            Client.Get (Path, Client.Response);

         when POST =>
            Client.Post (Path, "", Client.Response);

         when PUT =>
            Client.Put (Path, "", Client.Response);

         when PATCH =>
            Client.Patch (Path, "", Client.Response);

         when OPTIONS =>
            Client.Options (Path, Client.Response);

         when DELETE =>
            Client.Delete (Path, Client.Response);

      end case;
      if Client.Response.Get_Status /= Util.Http.SC_OK then
         Client_Type'Class (Client).Error (Client.Response.Get_Status, Client.Response);
         return;
      end if;
      declare
         Content_Type : constant String := Client.Response.Get_Header ("Content-Type");
      begin
         if Util.Http.Mimes.Is_Mime (Content_Type, Mime_Json.all) then
            declare
               Parser  : Util.Serialize.IO.JSON.Parser;
               Mapper  : Util.Beans.Objects.Readers.Reader;
               Content : constant String := Client.Response.Get_Body;
            begin
               if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
                  Log.Debug ("{0}", Content);
               end if;

               Parser.Parse_String (Content, Mapper);
               Reply := Mapper.Get_Root;
            end;
         elsif Util.Http.Mimes.Is_Mime (Content_Type, Mime_Xml.all)
           or else Content_Type = "text/xml"
         then
            declare
               Parser  : Util.Serialize.IO.XML.Parser;
               Mapper  : Util.Beans.Objects.Readers.Reader;
               Content : constant String := Client.Response.Get_Body;
            begin
               if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
                  Log.Debug ("{0}", Content);
               end if;

               Parser.Parse_String (Content, Mapper);
               Reply := Mapper.Get_Root;
            end;
         elsif Util.Http.Mimes.Is_Mime (Content_Type, Mime_Form.all) then
            declare
               Parser  : Util.Serialize.IO.Form.Parser;
               Mapper  : Util.Beans.Objects.Readers.Reader;
               Content : constant String := Client.Response.Get_Body;
            begin
               if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
                  Log.Debug ("{0}", Content);
               end if;

               Parser.Parse_String (Content, Mapper);
               Reply := Mapper.Get_Root;
            end;
         elsif Util.Http.Mimes.Is_Mime (Content_Type, Mime_Text.all) then
            declare
               Content : constant String := Client.Response.Get_Body;
            begin
               if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
                  Log.Debug ("{0}", Content);
               end if;

               Reply := Util.Beans.Objects.To_Object (Content);
            end;
         else
            declare
               Content : constant Util.Blobs.Blob_Ref := Client.Response.Get_Body;
            begin
               Reply := Util.Beans.Objects.To_Object (Content);
            end;
         end if;
      end;
   end Call;

   procedure Call (Client    : in out Client_Type;
                   Operation : in Operation_Type;
                   URI       : in URI_Type'Class;
                   Request   : in Request_Type'Class;
                   Reply     : out Value_Type) is
      Parser   : Util.Serialize.IO.JSON.Parser;
      Mapper   : Util.Beans.Objects.Readers.Reader;
      Path     : constant String := To_String (Client.Server) & To_String (URI);
   begin
      Request.Data.End_Document;
      if Client.Credential /= null then
         Client.Credential.Set_Credentials (Client);
      end if;
      declare
         Data     : constant String := Util.Streams.Texts.To_String (Request.Buffer);
      begin
         case Operation is
         when HEAD =>
            Log.Debug ("HEAD {0}", Path);
            Client.Head (Path, Client.Response);

         when GET =>
            Log.Debug ("GET {0}", Path);
            Client.Get (Path, Client.Response);

         when POST =>
            Log.Debug ("POST {0} {1}", Path, Data);
            Client.Post (Path, Data, Client.Response);

         when PUT =>
            Log.Debug ("PUT {0} {1}", Path, Data);
            Client.Put (Path, Data, Client.Response);

         when PATCH =>
            Log.Debug ("PATCH {0} {1}", Path, Data);
            Client.Patch (Path, Data, Client.Response);

         when OPTIONS =>
            Log.Debug ("OPTIONS {0}", Path);
            Client.Options (Path, Client.Response);

         when DELETE =>
            Log.Debug ("DELETE {0}", Path);
            Client.Delete (Path, Client.Response);

         end case;
      end;
      if Client.Response.Get_Status /= Util.Http.SC_OK then
         Client_Type'Class (Client).Error (Client.Response.Get_Status, Client.Response);
         return;
      end if;
      --  Todo check Response.Get_Header ("Content-Type")
      Parser.Parse_String (Client.Response.Get_Body, Mapper);
      if Log.Get_Level >= Util.Log.DEBUG_LEVEL then
         Log.Debug ("{0}", Client.Response.Get_Body);
      end if;
      Reply := Mapper.Get_Root;
   end Call;

   --  ------------------------------
   --  Set the Accept header according to what the operation supports and what is
   --  selected by the client.
   --  ------------------------------
   procedure Set_Accept (Client : in out Client_Type;
                         List   : in Mime_List) is
      Header : UString;
   begin
      for Content_Type of List loop
         if Length (Header) > 0 then
            Append (Header, ", ");
         end if;
         Append (Header, Content_Type.all);
      end loop;
      Client.Set_Header ("Accept", To_String (Header));
   end Set_Accept;

   --  ------------------------------
   --  Handle an error after an API call.  The default implementation raises an exception
   --  if the HTTP status code is 400, 401 or 403.
   --  ------------------------------
   procedure Error (Client   : in out Client_Type;
                    Status   : in Natural;
                    Response : in Util.Http.Clients.Response'Class) is
      pragma Unreferenced (Client, Response);
   begin
      if Status = 404 then
         raise Not_Found;
      elsif Status = 401 then
         raise Authorization_Error;
      elsif Status = 403 then
         raise Permission_Error;
      elsif Status >= 400 then
         raise Parameter_Error;
      end if;
   end Error;

   --  ------------------------------
   --  Get the HTTP response code status.
   --  ------------------------------
   function Get_Status (Client : in Client_Type) return Natural is
   begin
      return Client.Response.Get_Status;
   end Get_Status;

   --  ------------------------------
   --  Initialize the request body to prepare for the serialization of data using
   --  a supported and configured content type.
   --  ------------------------------
   procedure Initialize (Client  : in out Client_Type;
                         Request : in out Request_Type'Class;
                         Mimes   : in Mime_List) is
      Json  : Json_Output_Stream_Access;
      Forms : Form_Output_Stream_Access;
      Mime  : constant Mime_Access := Mimes (Mimes'First);
   begin
      Client.Set_Header ("Content-Type", Mime.all);
      if Mime = Util.Http.Mimes.Form'Access then
         Request.Buffer.Initialize (Size => 1000000);
         Forms := new OpenAPI.Streams.Forms.Output_Stream;
         Request.Data := Forms;
         Forms.Initialize (Request.Buffer'Unchecked_Access);
         Request.Data.Start_Document;

      elsif Mime = Util.Http.Mimes.Json'Access then
         Client.Set_Header ("Content-Type", Mime.all);
         Json := new Util.Serialize.IO.JSON.Output_Stream;
         Request.Data := Json;
         Request.Buffer.Initialize (Size => 1000000);
         Json.Initialize (Request.Buffer'Unchecked_Access);
         Request.Data.Start_Document;
         Request.Data.Start_Entity ("");
      end if;
   end Initialize;

end OpenAPI.Clients;
