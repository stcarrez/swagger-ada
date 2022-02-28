-----------------------------------------------------------------------
--  openapi-credentials-oauth -- OAuth2 client credentials
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

private

   type OAuth2_Credential_Type is new Security.OAuth.Clients.Application
     and Credential_Type with record
      Token  : Security.OAuth.Clients.Grant_Type;
      Scope  : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end OpenAPI.Credentials.OAuth;
