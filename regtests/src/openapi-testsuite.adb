-----------------------------------------------------------------------
--  openapi-testsuite - Swagger Test suite
--  Copyright (C) 2017, 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with OpenAPI.Clients.Tests;
with OpenAPI.Tests;
package body OpenAPI.Testsuite is

   Tests : aliased Util.Tests.Test_Suite;

   function Suite return Util.Tests.Access_Test_Suite is
      Ret : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
      OpenAPI.Clients.Tests.Add_Tests (Ret);
      if Util.Tests.Get_Parameter ("testapi.url") /= "" then
         OpenAPI.Tests.Add_Tests (Ret);
      end if;
      return Ret;
   end Suite;

end OpenAPI.Testsuite;
