--  ------------ EDIT NOTE ------------
--  REST API Validation
--  API to validate
--  This file was generated with openapi-generator.  You can modify it to implement
--  the client.  After you modify this file, you should add the following line
--  to the .openapi-generator-ignore file:
--
--  src/testapi.ads
--
--  Then, you can drop this edit note comment.
--  ------------ EDIT NOTE ------------
with TestAPI.Clients;
with TestAPI.Models;
with OpenAPI;
with OpenAPI.Credentials.OAuth;
with Util.Http.Clients.Curl;
with Util.Log.Loggers;
with Util.Properties;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Calendar.Formatting;
with Ada.Exceptions;
procedure TestAPI.Client is

   use Ada.Text_IO;

   procedure Usage;

   Server : constant OpenAPI.UString :=
     OpenAPI.To_UString ("http://localhost:8080/v2");
   CONFIG_PATH : constant String  := "testapi.properties";
   Arg_Count   : constant Natural := Ada.Command_Line.Argument_Count;
   Arg         : Positive         := 1;
   Props       : Util.Properties.Manager;

   procedure Usage is
   begin
      Put_Line ("Usage: TestAPI {params}...");
   end Usage;

begin
   if Arg_Count <= 1 then
      Usage;
      return;
   end if;
   Props.Load_Properties (CONFIG_PATH);
   Util.Log.Loggers.Initialize (Props);
   Util.Http.Clients.Curl.Register;
   declare
      Command : constant String := Ada.Command_Line.Argument (Arg);
      Item    : constant String := Ada.Command_Line.Argument (Arg + 1);
      Cred    : aliased OpenAPI.Credentials.OAuth.OAuth2_Credential_Type;
      C       : TestAPI.Clients.Client_Type;
   begin
      C.Set_Server (Server);
      C.Set_Credentials (Cred'Unchecked_Access);
      Arg := Arg + 2;

   exception
      when E : Constraint_Error =>
         Put_Line
           ("Constraint error raised: " &
            Ada.Exceptions.Exception_Message (E));

   end;
end TestAPI.Client;
