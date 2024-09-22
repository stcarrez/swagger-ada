-----------------------------------------------------------------------
--  openapi-server-operation -- Rest server operation
--  Copyright (C) 2017, 2018, 2019, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
