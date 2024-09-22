-----------------------------------------------------------------------
--  openapi-credentials -- Rest client credentials
--  Copyright (C) 2018, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Http.Clients;

--  == REST Credentials ==
--  The <tt>Swagger.Credentials</tt> package provides support to represent client
--  credentials and add them when a REST operation is called.
package OpenAPI.Credentials is

   type Credential_Type is limited interface;

   --  Set the credentials on the HTTP client request before doing the call.
   procedure Set_Credentials (Credential : in Credential_Type;
                              Into       : in out Util.Http.Clients.Client'Class) is abstract;

end OpenAPI.Credentials;
