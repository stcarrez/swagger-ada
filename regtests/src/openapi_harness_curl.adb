-----------------------------------------------------------------------
--  openapi_harness -- Unit tests
--  Copyright (C) 2017, 2019, 2024 Stephane Carrez
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

with OpenAPI.Testsuite;
with Util.Tests;
with Util.Http.Clients.Curl;

procedure OpenAPI_Harness_Curl is
   procedure Harness is new Util.Tests.Harness (OpenAPI.Testsuite.Suite);
begin
   Util.Http.Clients.Curl.Register;
   Harness ("openapi-tests.xml");
end OpenAPI_Harness_Curl;