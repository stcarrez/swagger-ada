-----------------------------------------------------------------------
--  swagger-server-applications -- REST application
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
with Util.Properties;
private with ASF.Servlets.Rest;
private with ASF.Servlets.Files;
package Swagger.Servers.Applications is

   type Application_Type is limited new Swagger.Servers.Application_Type with private;

   --  Configures the REST application so that it is ready to handler REST
   --  operations as well as give access to the Swagger UI that describes them.
   not overriding
   procedure Configure (App    : in out Application_Type;
                        Config : in Util.Properties.Manager'Class);

private

   type Application_Type is limited new Swagger.Servers.Application_Type with record
      Api      : aliased ASF.Servlets.Rest.Rest_Servlet;
      Files    : aliased ASF.Servlets.Files.File_Servlet;
      Ui_Files : aliased ASF.Servlets.Files.File_Servlet;
   end record;

end Swagger.Servers.Applications;
