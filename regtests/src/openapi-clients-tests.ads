-----------------------------------------------------------------------
--  openapi-clients-tests -- Unit tests for clients
--  Copyright (C) 2017, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package OpenAPI.Clients.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test Set_Path operations.
   procedure Test_Set_Path (T : in out Test);

   --  Test Add_Param operations.
   procedure Test_Add_Param (T : in out Test);

end OpenAPI.Clients.Tests;
