-----------------------------------------------------------------------
--  openapi-server-operation -- Rest server operation
--  Copyright (C) 2017, 2018, 2019, 2022 Stephane Carrez
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

with Servlet.Rest.Operation;
package body OpenAPI.Servers.Operation is

   procedure Op (Req     : in out OpenAPI.Servers.Request'Class;
                 Reply   : in out OpenAPI.Servers.Response'Class;
                 Stream  : in out OpenAPI.Servers.Output_Stream'Class);

   procedure Op (Req     : in out OpenAPI.Servers.Request'Class;
                 Reply   : in out OpenAPI.Servers.Response'Class;
                 Stream  : in out OpenAPI.Servers.Output_Stream'Class) is
      Ctx : Context_Type;
   begin
      Ctx.Initialize (Req, Reply, Stream);
      Handler (Req, Reply, Stream, Ctx);

   exception
      when others =>
         Ctx.Set_Error (500, "Internal server error");
   end Op;

   package API is new Servlet.Rest.Operation (Handler    => Op'Access,
                                              Method     => Method,
                                              URI        => URI,
                                              Permission => Permission,
                                              Mimes      => Mimes);

   function Definition return Descriptor_Access is
   begin
      return API.Definition;
   end Definition;

end OpenAPI.Servers.Operation;
