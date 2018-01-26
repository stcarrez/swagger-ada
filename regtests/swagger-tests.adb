-----------------------------------------------------------------------
--  swagger-tests -- Unit tests for REST clients
--  Copyright (C) 2018 Stephane Carrez
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

with Util.Log.Loggers;
with Util.Test_Caller;
with Swagger.Clients;
with TestAPI.Clients;
with Ada.Text_IO;
with TestAPI.Models;
package body Swagger.Tests is

   Log   : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Swagger.Tests");

   package Caller is new Util.Test_Caller (Test, "Swagger.Clients");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test unauthorized access",
                       Test_Unauthorized'Access);
      Caller.Add_Test (Suite, "Test authorized access",
                       Test_Authorized'Access);
   end Add_Tests;

   overriding
   procedure Set_Up (T : in out Test) is
   begin
      T.Server := To_UString (Util.Tests.Get_Parameter ("testapi.url"));
   end Set_Up;

   procedure Configure (T : in out Test;
                        Client : in out TestAPI.Clients.Client_Type) is
   begin
      Client.Set_Server (To_String (T.Server));
      Client.Set_Server ("http://localhost:8080/v1");
   end Configure;

   procedure Authenticate (T    : in out Test;
                           Cred : in out Swagger.Credentials.OAuth.OAuth2_Credential_Type) is
      Username   : constant String := Util.Tests.Get_Parameter ("testapi.username");
      Password   : constant String := Util.Tests.Get_Parameter ("testapi.password");
      Client_Id  : constant String := Util.Tests.Get_Parameter ("testapi.client_id");
      Client_Sec : constant String := Util.Tests.Get_Parameter ("testapi.client_secret");
   begin
      Cred.Set_Application_Identifier (Client_Id);
      Cred.Set_Application_Secret (Client_Sec);
      Cred.Set_Provider_URI (To_String (T.Server) & "/oauth/token");
      Cred.Request_Token (Username, Password, "read-ticket,write-ticket");
   end Authenticate;

   --  ------------------------------
   --  Test unauthorized operations.
   --  ------------------------------
   procedure Test_Unauthorized (T : in out Test) is
      Client : TestAPI.Clients.Client_Type;
      Empty  : Nullable_UString;
   begin
      T.Configure (Client);
      Client.Do_Create_Ticket (To_UString ("test"), Empty, Empty, Empty);
      T.Fail ("No authorization error exception was raised");

   exception
      when Swagger.Clients.Authorization_Error =>
         null;
   end Test_Unauthorized;

   --  ------------------------------
   --  Test authorized operations.
   --  ------------------------------
   procedure Test_Authorized (T : in out Test) is
      Client : TestAPI.Clients.Client_Type;
      Empty  : Nullable_UString;
      Cred   : aliased Swagger.Credentials.OAuth.OAuth2_Credential_Type;
      List   : TestAPI.Models.Ticket_Type_Vectors.Vector;
      Count  : Natural;
   begin
      T.Configure (Client);
      T.Authenticate (Cred);
      Client.Set_Credentials (Cred'Access);
      Client.Do_Create_Ticket (To_UString ("test"), Empty, Empty, Empty);
      Util.Tests.Assert_Equals (T, 201, Client.Get_Status, "Invalid response status");

      Ada.Text_IO.Put_Line (Client.Get_Header ("Location"));
      Client.Do_List_Tickets (Empty, Empty, List);
      Util.Tests.Assert_Equals (T, 200, Client.Get_Status, "Invalid response status");
      for Ticket of List loop
         Log.Info ("Ticket {0} - {1}", To_String (Ticket.Title), To_String (Ticket.Status));

         Client.Do_Update_Ticket (Tid         => Ticket.Id,
                                  Owner       => Ticket.Owner,
                                  Status      => (Is_Null => False, Value => To_UString ("closed")),
                                  Title       => (Is_Null => True, Value => <>),
                                  Description => (Is_Null => False, Value => To_UString ("ok")),
                                  Result      => Ticket);
         Util.Tests.Assert_Equals (T, 200, Client.Get_Status, "Invalid response status");

         Util.Tests.Assert_Equals (T, "closed", To_String (Ticket.Status),
                                   "Invalid status after Update_Ticket");
         Util.Tests.Assert_Equals (T, "ok", To_String (Ticket.Description),
                                   "Invalid description after Update_Ticket");
      end loop;
      Count := Natural (List.Length);
      Log.Info ("Number of tickets{0}", Natural'Image (Count));
      Client.Do_List_Tickets (Empty, Empty, List);
      Util.Tests.Assert_Equals (T, 200, Client.Get_Status, "Invalid response status");

      Count := Natural (List.Length);
      Log.Info ("Number of tickets{0}", Natural'Image (Count));

      --  Delete each ticket, doing a DELETE operation.
      for Ticket of List loop
         Client.Do_Delete_Ticket (Tid => Ticket.Id);
         Util.Tests.Assert_Equals (T, 204, Client.Get_Status, "Invalid response status");
      end loop;
      Client.Do_List_Tickets (Empty, Empty, List);
      Util.Tests.Assert_Equals (T, 200, Client.Get_Status, "Invalid response status");

      Count := Natural (List.Length);
      Util.Tests.Assert_Equals (T, 0, Count, "The ticket list was not cleared");
   end Test_Authorized;

end Swagger.Tests;
