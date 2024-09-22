-----------------------------------------------------------------------
--  openapi-server-operation -- Rest server operation
--  Copyright (C) 2017, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Security.Permissions;
generic
   with procedure Handler (Req     : in out OpenAPI.Servers.Request'Class;
                           Reply   : in out OpenAPI.Servers.Response'Class;
                           Stream  : in out OpenAPI.Servers.Output_Stream'Class;
                           Context : in out OpenAPI.Servers.Context_Type);
   Method     : Method_Type := Servlet.Rest.GET;
   URI        : String;
   Permission : Security.Permissions.Permission_Index := Security.Permissions.NONE;
   Mimes      : Mime_List_Access;
package OpenAPI.Servers.Operation is

   function Definition return Descriptor_Access;

end OpenAPI.Servers.Operation;
