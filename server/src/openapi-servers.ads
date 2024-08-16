-----------------------------------------------------------------------
--  openapi-server -- Rest server support
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
with Servlet.Rest;
with Servlet.Core;
with Security.Permissions;
package OpenAPI.Servers is

   subtype Application_Type is Servlet.Core.Servlet_Registry;

   subtype Request is Servlet.Rest.Request;

   subtype Response is Servlet.Rest.Response;

   subtype Output_Stream is Servlet.Rest.Output_Stream;

   subtype Method_Type is Servlet.Rest.Method_Type;

   GET     : constant Method_Type := Servlet.Rest.GET;
   POST    : constant Method_Type := Servlet.Rest.POST;
   DELETE  : constant Method_Type := Servlet.Rest.DELETE;
   PUT     : constant Method_Type := Servlet.Rest.PUT;
   HEAD    : constant Method_Type := Servlet.Rest.HEAD;
   OPTIONS : constant Method_Type := Servlet.Rest.OPTIONS;
   PATCH   : constant Method_Type := Servlet.Rest.PATCH;

   subtype Descriptor_Access is Servlet.Rest.Descriptor_Access;

   --  Get a request parameter defined in the URI path.
   procedure Get_Path_Parameter (Req   : in Request'Class;
                                 Pos   : in Positive;
                                 Value : out UString);

   --  Get a request parameter defined in the URI path.
   procedure Get_Path_Parameter (Req   : in Request'Class;
                                 Pos   : in Positive;
                                 Value : out Long);
   function Get_Path_Parameter (Req   : in Request'Class;
                                Pos   : in Positive) return String;

   --  Get a request parameter from the query string.
   function Get_Query_Parameter (Req   : in Request'Class;
                                 Name  : in String) return String;

   procedure Get_Query_Parameter (Req   : in Request'Class;
                                  Name  : in String;
                                  Value : out UString);
   procedure Get_Query_Parameter (Req   : in Request'Class;
                                  Name  : in String;
                                  Value : out Nullable_UString);

   --  Get a request parameter from the query string.
   procedure Get_Query_Parameter (Req   : in Request'Class;
                                  Name  : in String;
                                  Value : out UString_Vectors.Vector);
   procedure Get_Query_Parameter (Req   : in Request'Class;
                                  Name  : in String;
                                  Value : out Nullable_UString_Vectors.Vector);

   --  Get a request parameter from the query as boolean.
   procedure Get_Query_Parameter (Req   : in Request'Class;
                                  Name  : in String;
                                  Value : out Boolean);
   procedure Get_Query_Parameter (Req   : in Request'Class;
                                  Name  : in String;
                                  Value : out Nullable_Boolean);

   --  Read the request body and get a value object tree.
   procedure Read (Req      : in Request'Class;
                   Consumes : in Mime_List;
                   Value    : out Value_Type);

   type Context_Type is tagged limited private;

   procedure Initialize (Context : in out Context_Type;
                         Req     : in out Request'Class;
                         Reply   : in out Response'Class;
                         Stream  : in out Output_Stream'Class);

   procedure Register (Registry   : in out Servlet.Core.Servlet_Registry'Class;
                       Definition : in Descriptor_Access)
     renames Servlet.Rest.Register;

   --  Get a request parameter passed in the form.
   procedure Get_Parameter (Req   : in out Context_Type;
                            Name  : in String;
                            Value : out Long);

   --  Get a request parameter passed in the form.
   procedure Get_Parameter (Req   : in out Context_Type;
                            Name  : in String;
                            Value : out Integer);

   --  Get a request parameter passed in the form.
   procedure Get_Parameter (Req   : in out Context_Type;
                            Name  : in String;
                            Value : out UString);
   procedure Get_Parameter (Req   : in out Context_Type;
                            Name  : in String;
                            Value : out Nullable_UString);

   --  Get a request parameter passed in the form.
   procedure Get_Parameter (Req   : in out Context_Type;
                            Name  : in String;
                            Value : out Boolean);

   --  Set the response error code with a message to return.
   procedure Set_Error (Context : in out Context_Type;
                        Code    : in Natural;
                        Message : in String);

   --  Set the HTTP status in the response.
   procedure Set_Status (Context : in out Context_Type;
                         Code    : in Natural);

   --  Set the response description in the X-OpenAPI-Message header when enabled.
   procedure Set_Description (Context : in out Context_Type;
                              Message : in String);

   --  Send a Location: header in the response.
   procedure Set_Location (Context : in out Context_Type;
                           URL     : in String);

   --  Get the HTTP status that will be sent in the response.
   function Get_Status (Context : in Context_Type) return Natural;

   --  Returns True if the API request is authenticated.
   function Is_Authenticated (Context : in Context_Type) return Boolean;

   --  Returns True if the client doing the request has the given permission.
   function Has_Permission (Context    : in Context_Type;
                            Permission : in Security.Permissions.Permission_Index) return Boolean;

private

   function Get_Parameter (Req : in out Context_Type;
                           Name : in String) return String;
   procedure Read (Context : in out Context_Type);

   type Context_Type is tagged limited record
      Req     : access Request'Class;
      Reply   : access Response'Class;
      Stream  : access Output_Stream'Class;
      Params  : Util.Beans.Objects.Object;
      Use_Map : Boolean := False;
   end record;

end OpenAPI.Servers;
