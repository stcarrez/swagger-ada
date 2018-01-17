-----------------------------------------------------------------------
--  swagger-credentials -- Rest client credentials
--  Copyright (C) 2018 Stephane Carrez
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

--  == REST Credentials ==
--  The <tt>Swagger.Credentials</tt> package provides support to represent client
--  credentials and add them when a REST operation is called.
package Swagger.Credentials is

   type Credential_Type is limited interface;

   --  Set the credendials on the HTTP client request before doing the call.
   procedure Set_Credentials (Credential : in Credential_Type;
                              Into       : in out Util.Http.Clients.Client'Class) is abstract;

end Swagger.Credentials;
