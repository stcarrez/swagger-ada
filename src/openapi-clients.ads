-----------------------------------------------------------------------
--  openapi-clients -- Rest client support
--  Copyright (C) 2017, 2018, 2020, 2022 Stephane Carrez
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
with Util.Http.Clients;
private with Util.Streams.Texts;
with OpenAPI.Streams;
with OpenAPI.Credentials;
with Util.Serialize.IO;

--  == REST Client ==
--  The <tt>OpenAPI.Clients</tt> package implements the support used by the code generator
--  to make REST client operations.
package OpenAPI.Clients is

   --  Exception raised when an API was not found.
   Not_Found           : exception;

   --  Exception raised when a parameter is invalid.
   Parameter_Error     : exception;

   --  Exception raised when the caller is not authorized.
   Authorization_Error : exception;

   --  Exception raised when the caller does not have the permission.
   Permission_Error    : exception;

   type Request_Type is tagged limited private;

   type Stream_Accessor (Stream : access OpenAPI.Streams.Output_Stream'Class) is private
   with Implicit_Dereference => Stream;

   function Stream (Req : in Request_Type) return Stream_Accessor;

   type Operation_Type is (HEAD, GET, POST, PUT, DELETE, OPTIONS, PATCH);

   type URI_Type is tagged private;

   --  Set the path to use for the URI.
   procedure Set_Path (URI  : in out URI_Type;
                       Path : in String);

   --  Set the path parameter.
   procedure Set_Path_Param (URI   : in out URI_Type;
                             Name  : in String;
                             Value : in String);

   --  Set the path parameter.
   procedure Set_Path_Param (URI   : in out URI_Type;
                             Name  : in String;
                             Value : in UString);

   --  Add a query parameter.
   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in String);

   --  Add a query parameter.
   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in UString);
   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in Nullable_UString);

   --  Add a query parameter.
   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in Boolean);
   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in Nullable_Boolean);

   --  Add a query parameter.
   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in UString_Vectors.Vector);
   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in Nullable_UString_Vectors.Vector);

   --  Convert the URI into a string.
   function To_String (URI : in URI_Type) return String;

   type Client_Type is new Util.Http.Clients.Client with private;

   --  Set the server base URI to connect to.
   procedure Set_Server (Client : in out Client_Type;
                         Server : in String);
   procedure Set_Server (Client : in out Client_Type;
                         Server : in UString);

   --  Set the credential instance that is responsible for populating the HTTP request
   --  before sending the request.
   procedure Set_Credentials (Client     : in out Client_Type;
                              Credential : access OpenAPI.Credentials.Credential_Type'Class);

   procedure Call (Client    : in out Client_Type;
                   Operation : in Operation_Type;
                   URI       : in URI_Type'Class;
                   Request   : in Request_Type'Class);

   procedure Call (Client    : in out Client_Type;
                   Operation : in Operation_Type;
                   URI       : in URI_Type'Class);

   procedure Call (Client    : in out Client_Type;
                   Operation : in Operation_Type;
                   URI       : in URI_Type'Class;
                   Reply     : out Value_Type);

   procedure Call (Client    : in out Client_Type;
                   Operation : in Operation_Type;
                   URI       : in URI_Type'Class;
                   Request   : in Request_Type'Class;
                   Reply     : out Value_Type);

   --  Set the Accept header according to what the operation supports and what is
   --  selected by the client.
   procedure Set_Accept (Client : in out Client_Type;
                         List   : in Mime_List);

   --  Handle an error after an API call.  The default implementation raises an exception
   --  if the HTTP status code is 400, 401 or 403.
   procedure Error (Client   : in out Client_Type;
                    Status   : in Natural;
                    Response : in Util.Http.Clients.Response'Class);

   --  Get the HTTP response code status.
   function Get_Status (Client : in Client_Type) return Natural;

   --  Initialize the request body to prepare for the serialization of data using
   --  a supported and configured content type.
   procedure Initialize (Client  : in out Client_Type;
                         Request : in out Request_Type'Class;
                         Mimes   : in Mime_List);

private

   type Request_Type is tagged limited record
      Buffer : aliased Util.Streams.Texts.Print_Stream;
      Data   : access Util.Serialize.IO.Output_Stream'Class;
   end record;

   type Client_Type is new Util.Http.Clients.Client with record
      Server     : UString;
      Credential : access OpenAPI.Credentials.Credential_Type'Class;
      Response   : Util.Http.Clients.Response;
   end record;

   type Stream_Accessor (Stream : access OpenAPI.Streams.Output_Stream'Class) is record
      N : Natural := 0;
   end record;

   type URI_Type is tagged record
      URI   : UString;
      Query : UString;
   end record;

end OpenAPI.Clients;
