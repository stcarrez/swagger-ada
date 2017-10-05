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
with Util.Http.Clients;
private with Util.Streams.Texts;
with Swagger.Streams;
with Util.Serialize.IO;
package Swagger.Clients is

   type Request_Type is tagged limited private;

   type Stream_Accessor (Stream : access Swagger.Streams.Output_Stream'Class) is private
   with Implicit_Dereference => Stream;

   function Stream (Req : in Request_Type) return Stream_Accessor;

   type Operation_Type is (GET, POST, PUT, DELETE);

   --  The possible content types that are supported by the Swagger Ada client library.
   type Content_Type is (APPLICATION_JSON, APPLICATION_XML, APPLICATION_FORM);

   --  A list of content types for the Set_Accept and Initialize operations.
   type Content_Type_Array is array (Positive range <>) of Content_Type;

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

   --  Add a query parameter.
   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in UString_Vectors.Vector);

   --  Convert the URI into a string.
   function To_String (URI : in URI_Type) return String;

   type Client_Type is new Util.Http.Clients.Client with private;

   --  Set the server base URI to connect to.
   procedure Set_Server (Client : in out Client_Type;
                         Server : in String);

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
                         List   : in Content_Type_Array);

   --  Initialize the request body to prepare for the serialization of data using
   --  a supported and configured content type.
   procedure Initialize (Client  : in out Client_Type;
                         Request : in out Request_Type'Class;
                         Types   : in Content_Type_Array);

private

   type Request_Type is tagged limited record
      Buffer : aliased Util.Streams.Texts.Print_Stream;
      Data   : access Util.Serialize.IO.Output_Stream'Class;
   end record;

   type Client_Type is new Util.Http.Clients.Client with record
      Server : UString;
   end record;

   type Stream_Accessor (Stream : access Swagger.Streams.Output_Stream'Class) is record
      N : Natural := 0;
   end record;

   type URI_Type is tagged record
      URI   : UString;
      Query : UString;
   end record;

end Swagger.Clients;
