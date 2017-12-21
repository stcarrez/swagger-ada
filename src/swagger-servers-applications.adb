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

package body Swagger.Servers.Applications is

   --  ------------------------------
   --  Configures the REST application so that it is ready to handler REST
   --  operations as well as give access to the Swagger UI that describes them.
   --  ------------------------------
   not overriding
   procedure Configure (App    : in out Application_Type;
                        Config : in Util.Properties.Manager'Class) is
   begin
      App.Set_Init_Parameters (Config);

      --  Register the servlets and filters
      App.Add_Servlet (Name => "api", Server => App.Api'Unchecked_Access);
      App.Add_Servlet (Name => "files", Server => App.Files'Unchecked_Access);

      --  Define servlet mappings
      App.Add_Mapping (Name => "api", Pattern => "/*");
      App.Add_Mapping (Name => "files", Pattern => "/ui/*.html");
      App.Add_Mapping (Name => "files", Pattern => "/ui/*.js");
      App.Add_Mapping (Name => "files", Pattern => "/ui/*.png");
      App.Add_Mapping (Name => "files", Pattern => "/ui/*.css");
      App.Add_Mapping (Name => "files", Pattern => "/swagger/*.json");
      App.Add_Mapping (Name => "files", Pattern => "/ui/*.map");
   end Configure;

end Swagger.Servers.Applications;
