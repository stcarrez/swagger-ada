-----------------------------------------------------------------------
--  openapi-tests -- Unit tests for REST clients
--  Copyright (C) 2018, 2020, 2021, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Log.Loggers;
with Util.Test_Caller;
with Util.Assertions;
with Ada.Text_IO;
with TestAPI.Clients;
with TestAPI.Models;
with TestBinary.Clients;
with TestBinary.Models;
with Types.Clients;
with Types.Models;
with Enums.Clients;
with Enums.Models;
with External;
package body OpenAPI.Tests is

   Log   : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("OpenAPI.Tests");

   procedure Assert_Equals is
      new Util.Assertions.Assert_Equals_T (Enums.Models.Status_Type);

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
      Caller.Add_Test (Suite, "Test integer and floats in application/json response",
                       Test_Struct_Numbers'Access);
      Caller.Add_Test (Suite, "Test POST/PUT with JSON content",
                       Test_Consumes_JSON'Access);
      Caller.Add_Test (Suite, "Test enums",
                       Test_Enums'Access);
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

   --  Test API that uses a struct with various numbers.
   procedure Test_Struct_Numbers (T : in out Test) is
      Client : TestAPI.Clients.Client_Type;
      Result : TestAPI.Models.IntStruct_Type;
      Empty  : OpenAPI.Nullable_UString;
   begin
      T.Configure (Client);
      Client.Test_Int (Empty, Result);
      Util.Tests.Assert_Equals (T, 123456789, Natural (Result.Long_Int),
                                "Invalid Long_Int");
      Util.Tests.Assert_Equals (T, 12, Natural (Result.Short_Int),
                                "Invalid Long_Int");
      Util.Tests.Assert_Equals (T, 34, Natural (Result.Short_Int_2),
                                "Invalid Short_Int_2");
   end Test_Struct_Numbers;

   --  Test API that consumes a JSON content ('types.yaml')
   procedure Test_Consumes_JSON (T : in out Test) is
      Client : Types.Clients.Client_Type;
      R1     : Types.Models.RackInfo_Type;
      Result : Types.Models.RackInfo_Type;
      RA     : OpenAPI.Long;
      RB     : OpenAPI.Long;
   begin
      T.Configure (Client);

      Result.Id := 0;
      R1.Name := To_UString ("Rack A");
      R1.V := 10.0;
      Client.Add_Rack (R1, Result);
      T.Assert (Result.Id > 0, "Invalid id after Add_Rack");
      Util.Tests.Assert_Equals (T, "Rack A", Result.Name, "Invalid name");
      T.Assert (R1.V = Result.V, "Invalid value");
      RA := Result.Id;

      R1.Name := To_UString ("Rack B");
      R1.V := 20.0;
      Client.Add_Rack (R1, Result);
      T.Assert (Result.Id > 0, "Invalid id after Add_Rack");
      Util.Tests.Assert_Equals (T, "Rack B", Result.Name, "Invalid name");
      T.Assert (R1.V = Result.V, "Invalid value");
      RB := Result.Id;
      T.Assert (RA /= RB, "Must have different number for rack id");

      Client.Get_Rack (RA, Result);
      Util.Tests.Assert_Equals (T, "Rack A", Result.Name, "Invalid name");
      T.Assert (Result.V = 10.0, "Invalid rack A value");

      Client.Get_Rack (RB, Result);
      Util.Tests.Assert_Equals (T, "Rack B", Result.Name, "Invalid name");
      T.Assert (Result.V = 20.0, "Invalid rack B value");

      R1.Name := To_UString ("Rack B-updated");
      R1.V := 200.0;
      Client.Update_Rack (RB, R1, Result);
      Util.Tests.Assert_Equals (T, "Rack B-updated", Result.Name, "Invalid name");

   end Test_Consumes_JSON;

   procedure Test_Enums (T : in out Test) is
      procedure Check (Status : in Enums.Models.Status_Type);

      Client : Enums.Clients.Client_Type;
      List   : Enums.Models.Stat_Type_Vectors.Vector;

      procedure Check (Status : in Enums.Models.Status_Type) is
      begin
         Client.Do_Get_Enums (Status, List);
         Util.Tests.Assert_Equals (T, 5, Natural (List.Length),
                                   "Invalid number of items with " & Status'Image);
         for I in 1 .. 5 loop
            declare
               Item : constant Enums.Models.Stat_Type := List.Element (I);
            begin
               Util.Tests.Assert_Equals (T, I, Natural (Item.Count),
                                         "Invalid Count");
               Assert_Equals (T, Status, Item.Status, "Invalid status");
            end;
         end loop;
      end Check;
   begin
      T.Configure (Client);

      Check (Enums.Models.OPEN);
      Check (Enums.Models.ONHOLD);
      Check (Enums.Models.ASSIGNED);
      Check (Enums.Models.ASSIGNED);
      Check (Enums.Models.CLOSED);
   end Test_Enums;

end OpenAPI.Tests;
