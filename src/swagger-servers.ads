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
with Util.Http.Clients;
private with Util.Streams.Texts;
with Swagger.Streams;
with Util.Serialize.IO;
with ASF.Rest;
with ASF.Requests;
with ASF.Responses;
with ASF.Streams;
with ASF.Servlets;
package Swagger.Servers is

   subtype Application_Type is ASF.Servlets.Servlet_Registry;

   subtype Request is ASF.Rest.Request;

   subtype Response is ASF.Rest.Response;

   subtype Output_Stream is ASF.Rest.Output_Stream;

   --  Get a request parameter defined in the URI path.
   procedure Get_Path_Parameter (Req   : in Request'Class;
                                 Pos   : in Positive;
                                 Value : out UString);

   --  Get a request parameter defined in the URI path.
   procedure Get_Path_Parameter (Req   : in Request'Class;
                                 Pos   : in Positive;
                                 Value : out Long);

end Swagger.Servers;
