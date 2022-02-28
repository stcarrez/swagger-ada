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

package body OpenAPI.Credentials.OAuth is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Set the credentials on the HTTP client request before doing the call.
   --  ------------------------------
   overriding
   procedure Set_Credentials (Credential : in OAuth2_Credential_Type;
                              Into       : in out Util.Http.Clients.Client'Class) is
   begin
      Into.Set_Header (Name  => "Authorization",
                       Value => "Bearer " & Credential.Token.Get_Name);
   end Set_Credentials;

   --  ------------------------------
   --  Request a OAuth token with username and password credential.
   --  Upon successful completion, the credential contains an access token that
   --  can be used to authorize REST operations.
   --  ------------------------------
   procedure Request_Token (Credential : in out OAuth2_Credential_Type;
                            Username   : in String;
                            Password   : in String;
                            Scope      : in String) is
   begin
      Credential.Scope := To_Unbounded_String (Scope);
      Credential.Request_Token (Username, Password, Scope, Credential.Token);
   end Request_Token;

   --  ------------------------------
   --  Refresh the OAuth access token with the refresh token.
   --  ------------------------------
   procedure Refresh_Token (Credential : in out OAuth2_Credential_Type) is
   begin
      Credential.Refresh_Token (To_String (Credential.Scope), Credential.Token);
   end Refresh_Token;

end OpenAPI.Credentials.OAuth;
