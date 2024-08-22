-----------------------------------------------------------------------
--  openapi-server-applications -- REST application
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
with Util.Properties;
private with Security.OAuth.Servers;
private with Servlet.Core.Rest;
private with Servlet.Core.Files;
private with Servlet.Security.OAuth;
private with Servlet.Security.Filters.OAuth;
private with Security.OAuth.File_Registry;
package OpenAPI.Servers.Applications is

   type Application_Type is limited new OpenAPI.Servers.Application_Type with private;

   --  Configures the REST application so that it is ready to handler REST
   --  operations as well as give access to the OpenAPI UI that describes them.
   not overriding
   procedure Configure (App    : in out Application_Type;
                        Config : in Util.Properties.Manager'Class);

private

   type Application_Type is limited new OpenAPI.Servers.Application_Type with record
      Api      : aliased Servlet.Core.Rest.Rest_Servlet;
      OAuth    : aliased Servlet.Security.OAuth.Token_Servlet;
      Filter   : aliased Servlet.Security.Filters.OAuth.Auth_Filter;
      Files    : aliased Servlet.Core.Files.File_Servlet;
      Ui_Files : aliased Servlet.Core.Files.File_Servlet;
      Auth     : aliased Security.OAuth.Servers.Auth_Manager;
      Apps     : aliased Security.OAuth.File_Registry.File_Application_Manager;
      Realm    : aliased Security.OAuth.File_Registry.File_Realm_Manager;
   end record;

end OpenAPI.Servers.Applications;
