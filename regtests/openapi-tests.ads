-----------------------------------------------------------------------
--  openapi-tests -- Unit tests for REST clients
--  Copyright (C) 2018, 2022 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Util.Tests;
with OpenAPI.Clients;
with OpenAPI.Credentials.OAuth;
package OpenAPI.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with record
      Server : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding
   procedure Set_Up (T : in out Test);

   procedure Configure (T : in out Test;
                        Client : in out OpenAPI.Clients.Client_Type'Class);

   procedure Authenticate (T    : in out Test;
                           Cred : in out OpenAPI.Credentials.OAuth.OAuth2_Credential_Type);

   --  Test unauthorized operations.
   procedure Test_Unauthorized (T : in out Test);

   --  Test authorized operations.
   procedure Test_Authorized (T : in out Test);

   --  Test API that uses text/plain response.
   procedure Test_Text_Response (T : in out Test);

   --  Test API that uses image/png response.
   procedure Test_Binary_Response (T : in out Test);

   --  Test API that uses an external data type.
   procedure Test_External_Data (T : in out Test);

end OpenAPI.Tests;
