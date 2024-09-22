-----------------------------------------------------------------------
--  openapi-tests -- Unit tests for REST clients
--  Copyright (C) 2018, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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

   --  Test API that uses a struct with various numbers.
   procedure Test_Struct_Numbers (T : in out Test);

end OpenAPI.Tests;
