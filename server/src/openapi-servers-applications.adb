-----------------------------------------------------------------------
--  openapi-server-applications -- REST application
--  Copyright (C) 2017, 2019, 2022 Stephane Carrez
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
with Util.Properties.Basic;
with OpenAPI.Servers.Config;
package body OpenAPI.Servers.Applications is

   --  ------------------------------
   --  Configures the REST application so that it is ready to handler REST
   --  operations as well as give access to the OpenAPI UI that describes them.
   --  ------------------------------
   not overriding
   procedure Configure (App    : in out Application_Type;
                        Config : in Util.Properties.Manager'Class) is
      use Util.Properties.Basic;

      Cfg        : Util.Properties.Manager;
      Dir        : constant String := Config.Get ("swagger.dir");
      UI_Enable  : constant Boolean := Boolean_Property.Get (Config, "swagger.ui.enable", True);
      Web_Enable : constant Boolean := Boolean_Property.Get (Config, "swagger.web.enable", True);
      Key        : constant String := Config.Get ("swagger.key");
   begin
      Cfg.Copy (Config);
      Cfg.Set ("view.dir", Dir & ";" & OpenAPI.Servers.Config.WEB_DIR);
      App.Set_Init_Parameters (Cfg);

      App.Realm.Load (Config, Config.Get ("swagger.users", "users"));
      App.Apps.Load (Config, Config.Get ("swagger.apps", "apps"));
      App.Filter.Set_Auth_Manager (App.Auth'Unchecked_Access);

      --  Configure the authorization manager.
      App.Auth.Set_Application_Manager (App.Apps'Unchecked_Access);
      App.Auth.Set_Realm_Manager (App.Realm'Unchecked_Access);
      App.OAuth.Set_Auth_Manager (App.Auth'Unchecked_Access);
      App.Auth.Set_Private_Key (Key);

      --  Register the servlets and filters
      App.Add_Filter (Name   => "oauth", Filter => App.Filter'Unchecked_Access);
      App.Add_Servlet (Name => "api", Server => App.Api'Unchecked_Access);
      App.Add_Servlet (Name => "files", Server => App.Files'Unchecked_Access);
      App.Add_Servlet (Name => "oauth", Server => App.OAuth'Unchecked_Access);

      --  Define servlet mappings
      App.Add_Mapping (Name => "api", Pattern => "/*");
      App.Add_Mapping (Name => "files", Pattern => "/swagger/*.json");
      App.Add_Mapping (Name => "oauth", Pattern => "/oauth/token");
      App.Add_Filter_Mapping (Name => "oauth", Pattern => "/*");
      if UI_Enable then
         App.Add_Mapping (Name => "files", Pattern => "/ui/*.html");
         App.Add_Mapping (Name => "files", Pattern => "/ui/*.js");
         App.Add_Mapping (Name => "files", Pattern => "/ui/*.png");
         App.Add_Mapping (Name => "files", Pattern => "/ui/*.css");
         App.Add_Mapping (Name => "files", Pattern => "/ui/*.map");
      end if;
      if Web_Enable then
         App.Add_Mapping (Name => "files", Pattern => "*.html");
         App.Add_Mapping (Name => "files", Pattern => "*.js");
         App.Add_Mapping (Name => "files", Pattern => "*.png");
         App.Add_Mapping (Name => "files", Pattern => "*.css");
         App.Add_Mapping (Name => "files", Pattern => "*.map");
         App.Add_Mapping (Name => "files", Pattern => "*.jpg");
      end if;
   end Configure;

end OpenAPI.Servers.Applications;
