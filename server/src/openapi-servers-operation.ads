-----------------------------------------------------------------------
--  openapi-server-operation -- Rest server operation
--  Copyright (C) 2017, 2018, 2022 Stephane Carrez
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
