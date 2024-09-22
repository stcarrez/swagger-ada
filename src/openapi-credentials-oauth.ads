-----------------------------------------------------------------------
--  openapi-credentials-oauth -- OAuth2 client credentials
--  Copyright (C) 2018, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Http.Clients;
with Security.OAuth.Clients;

package OpenAPI.Credentials.OAuth is

   type OAuth2_Credential_Type is new Security.OAuth.Clients.Application
     and Credential_Type with private;

   --  Set the credentials on the HTTP client request before doing the call.
   overriding
   procedure Set_Credentials (Credential : in OAuth2_Credential_Type;
                              Into       : in out Util.Http.Clients.Client'Class);

   --  Request a OAuth token with username and password credential.
   --  Upon successful completion, the credential contains an access token that
   --  can be used to authorize REST operations.
   procedure Request_Token (Credential : in out OAuth2_Credential_Type;
                            Username   : in String;
                            Password   : in String;
                            Scope      : in String);

   --  Refresh the OAuth access token with the refresh token.
   procedure Refresh_Token (Credential : in out OAuth2_Credential_Type);

   --  Set the bearer token to be used for the authentication.
   procedure Bearer_Token (Credential : in out OAuth2_Credential_Type;
                           Token      : in String);

private

   type OAuth2_Credential_Type is new Security.OAuth.Clients.Application
     and Credential_Type with record
      Token  : Security.OAuth.Clients.Grant_Type;
      Scope  : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end OpenAPI.Credentials.OAuth;
