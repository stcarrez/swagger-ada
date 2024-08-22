-----------------------------------------------------------------------
--  openapi-clients-tests -- Unit tests for clients
--  Copyright (C) 2017, 2022 Stephane Carrez
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
