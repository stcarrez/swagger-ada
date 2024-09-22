-----------------------------------------------------------------------
--  openapi_harness -- Unit tests
--  Copyright (C) 2017, 2019, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
