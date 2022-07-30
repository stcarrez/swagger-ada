-----------------------------------------------------------------------
--  openapi-tests -- Unit tests for REST clients
--  Copyright (C) 2018, 2020, 2021, 2022 Stephane Carrez
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
with Ada.Text_IO;
with TestAPI.Clients;
with TestAPI.Models;
with TestBinary.Clients;
with TestBinary.Models;
with External;
package body OpenAPI.Tests is

   Log   : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("OpenAPI.Tests");

   package Caller is new Util.Test_Caller (Test, "OpenAPI.Tests");

   procedure Verify_Get_Stats (T      : in out Test;
                               Result : in External.Stat_Vector;
                               Count  : in Natural;
                               Prefix : in String);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test unauthorized access",
                       Test_Unauthorized'Access);
      Caller.Add_Test (Suite, "Test authorized access",
                       Test_Authorized'Access);
      Caller.Add_Test (Suite, "Test text/plain response",
                       Test_Text_Response'Access);
      Caller.Add_Test (Suite, "Test image/png response",
                       Test_Binary_Response'Access);
      Caller.Add_Test (Suite, "Test application/json response",
                       Test_External_Data'Access);
   end Add_Tests;

   overriding
   procedure Set_Up (T : in out Test) is
   begin
      T.Server := To_UString (Util.Tests.Get_Parameter ("testapi.url"));
   end Set_Up;

   procedure Configure (T : in out Test;
                        Client : in out OpenAPI.Clients.Client_Type'Class) is
   begin
      Client.Set_Server (To_String (T.Server));
      Client.Set_Server (Util.Tests.Get_Parameter ("testapi.url"));
   end Configure;

   procedure Authenticate (T    : in out Test;
                           Cred : in out OpenAPI.Credentials.OAuth.OAuth2_Credential_Type) is
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
      when OpenAPI.Clients.Authorization_Error =>
         null;
   end Test_Unauthorized;

   --  ------------------------------
   --  Test authorized operations.
   --  ------------------------------
   procedure Test_Authorized (T : in out Test) is
      Client : TestAPI.Clients.Client_Type;
      Empty  : Nullable_UString;
      Cred   : aliased OpenAPI.Credentials.OAuth.OAuth2_Credential_Type;
      List   : TestAPI.Models.Ticket_Type_Vectors.Vector;
      Count  : Natural;
      T2     : TestAPI.Models.Ticket_Type;
   begin
      T.Configure (Client);
      T.Authenticate (Cred);
      Client.Set_Credentials (Cred'Unchecked_Access);
      Client.Do_Create_Ticket (To_UString ("test"), Empty, Empty, Empty);
      Util.Tests.Assert_Equals (T, 201, Client.Get_Status, "Invalid response status");

      Ada.Text_IO.Put_Line (Client.Get_Header ("Location"));
      Client.Do_List_Tickets (Empty, Empty, List);
      Util.Tests.Assert_Equals (T, 200, Client.Get_Status, "Invalid response status");

      Client.Do_Head_Ticket;
      Util.Tests.Assert_Equals (T, 200, Client.Get_Status, "Invalid response status");

      for Ticket of List loop
         Log.Info ("Ticket {0} - {1}", To_String (Ticket.Title), To_String (Ticket.Status));

         Client.Do_Patch_Ticket (Tid         => Ticket.Id,
                                 Owner       => Ticket.Owner,
                                 Status      => (Is_Null => False,
                                                 Value => To_UString ("assigned")),
                                 Title       => (Is_Null => True,
                                                 Value => <>),
                                 Description => (Is_Null => False,
                                                 Value => To_UString ("patch")),
                                 Result      => Ticket);
         Util.Tests.Assert_Equals (T, 200, Client.Get_Status, "Invalid response status");

         Client.Do_Update_Ticket (Tid         => Ticket.Id,
                                  Owner       => Ticket.Owner,
                                  Status      => (Is_Null => False,
                                                  Value => To_UString ("closed")),
                                  Title       => (Is_Null => True,
                                                  Value => <>),
                                  Description => (Is_Null => False,
                                                  Value => To_UString ("ok")),
                                  Result      => Ticket);
         Util.Tests.Assert_Equals (T, 200, Client.Get_Status, "Invalid response status");

         Util.Tests.Assert_Equals (T, "closed", To_String (Ticket.Status),
                                   "Invalid status after Update_Ticket");
         Util.Tests.Assert_Equals (T, "ok", To_String (Ticket.Description),
                                   "Invalid description after Update_Ticket");

         Client.Do_Get_Ticket (Tid => Ticket.Id,
                               Result => T2);
         Util.Tests.Assert_Equals (T, 200, Client.Get_Status, "Invalid response status");

         Client.Do_Options_Ticket (Tid => Ticket.Id,
                                   Result => T2);
         Util.Tests.Assert_Equals (T, 200, Client.Get_Status, "Invalid response status");

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

   --  Test API that uses text/plain response.
   procedure Test_Text_Response (T : in out Test) is
      Client : TestAPI.Clients.Client_Type;
      Options : Nullable_UString;
      Result : OpenAPI.UString;
   begin
      T.Configure (Client);
      Client.Test_Text_Response (Options, Result);
      Util.Tests.Assert_Equals (T, "text response: ", Result, "Invalid text/plain response");

      Options := (Is_Null => False, Value => OpenAPI.To_UString ("test content '<>;:"""));
      Client.Test_Text_Response (Options, Result);
      Util.Tests.Assert_Equals (T, "text response: test content '<>;:""",
                                Result, "Invalid text/plain response");
   end Test_Text_Response;

   --  Test API that uses text/plain response.
   procedure Test_Binary_Response (T : in out Test) is
      Client  : TestBinary.Clients.Client_Type;
      Options : Nullable_UString;
      Result  : OpenAPI.Blob_Ref;
   begin
      T.Configure (Client);
      Client.Do_Get_Image (TestBinary.Models.OPEN, Options, Result);
      T.Assert (not Result.Is_Null, "Invalid image/png response");
      Util.Tests.Assert_Equals (T, 10, Natural (Result.Value.Len),
                                "Invalid length");

      Client.Do_Get_Image (TestBinary.Models.ONHOLD, Options, Result);
      T.Assert (not Result.Is_Null, "Invalid image/png response");
      Util.Tests.Assert_Equals (T, 20, Natural (Result.Value.Len),
                                "Invalid length");

      Client.Do_Get_Image (TestBinary.Models.ASSIGNED, Options, Result);
      T.Assert (not Result.Is_Null, "Invalid image/png response");
      Util.Tests.Assert_Equals (T, 30, Natural (Result.Value.Len),
                                "Invalid length");

      Client.Do_Get_Image (TestBinary.Models.CLOSED, Options, Result);
      T.Assert (not Result.Is_Null, "Invalid image/png response");
      Util.Tests.Assert_Equals (T, 40, Natural (Result.Value.Len),
                                "Invalid length");

      Client.Do_Get_Image (TestBinary.Models.REJECTED, Options, Result);
      T.Assert (not Result.Is_Null, "Invalid image/png response");
      Util.Tests.Assert_Equals (T, 50, Natural (Result.Value.Len),
                                "Invalid length");

   end Test_Binary_Response;

   procedure Verify_Get_Stats (T      : in out Test;
                               Result : in External.Stat_Vector;
                               Count  : in Natural;
                               Prefix : in String) is
   begin
      Util.Tests.Assert_Equals (T, Count, Natural (Result.Length),
                                "Invalid length");
      for I in 1 .. Count loop
         declare
            Item : constant External.Stat_Type := Result.Element (I);
         begin
            Util.Tests.Assert_Equals (T, I, Item.Count, "Invalid count");
            Util.Tests.Assert_Equals (T, Prefix & Natural'Image (I),
                                      Item.Name);
         end;
      end loop;
   end Verify_Get_Stats;

   --  Test API that uses text/plain response.
   procedure Test_External_Data (T : in out Test) is
      Client  : TestBinary.Clients.Client_Type;
      Result  : External.Stat_Vector;
   begin
      T.Configure (Client);
      Client.Do_Get_Stats (TestBinary.Models.OPEN, Result);
      Verify_Get_Stats (T, Result, 1, "OPEN");

      Client.Do_Get_Stats (TestBinary.Models.ASSIGNED, Result);
      Verify_Get_Stats (T, Result, 2, "ASSIGNED");

      Client.Do_Get_Stats (TestBinary.Models.CLOSED, Result);
      Verify_Get_Stats (T, Result, 10, "CLOSED");

      Client.Do_Get_Stats (TestBinary.Models.ONHOLD, Result);
      Verify_Get_Stats (T, Result, 10, "ONHOLD");

      Client.Do_Get_Stats (TestBinary.Models.REJECTED, Result);
      Verify_Get_Stats (T, Result, 10, "REJECTED");

   end Test_External_Data;

end OpenAPI.Tests;
