-----------------------------------------------------------------------
--  openapi-clients-tests -- Unit tests for clients
--  Copyright (C) 2017, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;

package body OpenAPI.Clients.Tests is

   package Caller is new Util.Test_Caller (Test, "OpenAPI.Clients");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test OpenAPI.Clients.Set_Path",
                       Test_Set_Path'Access);
      Caller.Add_Test (Suite, "Test OpenAPI.Clients.Add_Param",
                       Test_Add_Param'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test Set_Path operations.
   --  ------------------------------
   procedure Test_Set_Path (T : in out Test) is
      URI : URI_Type;
   begin
      URI.Set_Path ("/admin/{user}");
      URI.Set_Path_Param ("user", "admin");
      Util.Tests.Assert_Equals (T, "/admin/admin", URI.To_String,
                                "To_String on URI is invalid");
   end Test_Set_Path;

   --  ------------------------------
   --  Test Add_Param operations.
   --  ------------------------------
   procedure Test_Add_Param (T : in out Test) is
      URI : URI_Type;
   begin
      URI.Set_Path ("/admin/list");
      URI.Add_Param ("status", "active");
      Util.Tests.Assert_Equals (T, "/admin/list?status=active", URI.To_String,
                                "To_String on URI is invalid");
   end Test_Add_Param;

end OpenAPI.Clients.Tests;
