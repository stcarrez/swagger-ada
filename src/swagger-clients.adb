-----------------------------------------------------------------------
--  swagger-clients -- Rest client support
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
with Ada.Text_IO;
with Util.Beans.Objects.Readers;
with Util.Serialize.IO.JSON;
with Util.Strings;
package body Swagger.Clients is

   use Ada.Strings.Unbounded;

   function Stream (Req : in Request_Type) return Stream_Accessor is
   begin
      return Stream_Accessor '(Stream => Req.Data.all'Access, N => 0);
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
      First : Natural := Path'First;
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
      Empty    : Request_Type;
      No_Reply : Value_Type;
   begin
      Client.Call (Operation, URI, Empty, No_Reply);
   end Call;

   procedure Call (Client    : in out Client_Type;
                   Operation : in Operation_Type;
                   URI       : in URI_Type'Class;
                   Reply     : out Value_Type) is
      Response : Util.Http.Clients.Response;
      Parser   : Util.Serialize.IO.JSON.Parser;
      Mapper   : Util.Beans.Objects.Readers.Reader;
      Path     : constant String := To_String (Client.Server) & To_String (URI);
   begin
      case Operation is
         when GET =>
            Client.Get (Path, Response);

         when POST =>
            Client.Post (Path, "", Response);

         when PUT =>
            Client.Put (Path, "", Response);

         when others =>
            raise Program_Error;

      end case;
      if Response.Get_Status /= Util.Http.SC_OK then
         return;
      end if;
      --  Todo check Response.Get_Header ("Content-Type")
      Parser.Parse_String (Response.Get_Body, Mapper);
      Ada.Text_IO.Put_Line (Response.Get_Body);
      --      Reply := Util.Beans.Objects.To_Object (Response.Get_Body);
      Reply := Mapper.Get_Root;
   end Call;

   procedure Call (Client    : in out Client_Type;
                   Operation : in Operation_Type;
                   URI       : in URI_Type'Class;
                   Request   : in Request_Type'Class;
                   Reply     : out Value_Type) is
      Response : Util.Http.Clients.Response;
      Parser   : Util.Serialize.IO.JSON.Parser;
      Mapper   : Util.Beans.Objects.Readers.Reader;
      Path     : constant String := To_String (Client.Server) & To_String (URI);
   begin
      Request.Data.End_Document;
      declare
         Data     : constant String := Util.Streams.Texts.To_String (Request.Buffer);
      begin
         Ada.Text_IO.Put_Line (Data);
         case Operation is
         when GET =>
            Client.Get (Path, Response);

         when POST =>
            Client.Post (Path, Data, Response);

         when PUT =>
            Client.Put (Path, Data, Response);

         when others =>
            raise Program_Error;

         end case;
      end;
      if Response.Get_Status /= Util.Http.SC_OK then
         return;
      end if;
      --  Todo check Response.Get_Header ("Content-Type")
      Parser.Parse_String (Response.Get_Body, Mapper);
      Ada.Text_IO.Put_Line (Response.Get_Body);
      --      Reply := Util.Beans.Objects.To_Object (Response.Get_Body);
      Reply := Mapper.Get_Root;
   end Call;

   --  ------------------------------
   --  Set the Accept header according to what the operation supports and what is
   --  selected by the client.
   --  ------------------------------
   procedure Set_Accept (Client : in out Client_Type;
                         List   : in Content_Type_Array) is
      Header : UString;
   begin
      for Content_Type of List loop
         if Length (Header) > 0 then
            Append (Header, ", ");
         end if;
         case Content_Type is
            when APPLICATION_XML =>
               null;  --  Append (Header, "application/xml");

            when APPLICATION_JSON =>
               Append (Header, "application/json");

            when APPLICATION_FORM =>
               Append (Header, "application/x-www-form-urlencoded");

         end case;
      end loop;
      Client.Set_Header ("Accept", To_String (Header));
   end Set_Accept;

   --  ------------------------------
   --  Initialize the request body to prepare for the serialization of data using
   --  a supported and configured content type.
   --  ------------------------------
   procedure Initialize (Client  : in out Client_Type;
                         Request : in out Request_Type'Class;
                         Types   : in Content_Type_Array) is
      Json : access Util.Serialize.IO.JSON.Output_Stream'Class;
   begin
      case Types (Types'First) is
         when APPLICATION_FORM =>
            Client.Set_Header ("Content-Type", "application/x-www-form-urlencoded");

         when APPLICATION_JSON =>
            Client.Set_Header ("Content-Type", "application/json");
            Json := new Util.Serialize.IO.JSON.Output_Stream;
            Request.Data := Json;
            Request.Buffer.Initialize (Size => 1000000);
            Json.Initialize (Request.Buffer'Unchecked_Access);
            Request.Data.Start_Document;

         when APPLICATION_XML =>
            Client.Set_Header ("Content-Type", "application/xml");

      end case;
      null;
   end Initialize;

end Swagger.Clients;
