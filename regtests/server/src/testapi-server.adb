with Ada.IO_Exceptions;
with AWS.Config.Set;
with Servlet.Server;
with OpenAPI.Servers.Applications;
with Util.Strings;
with Util.Log.Loggers;
with Util.Properties;
with Util.Properties.Basic;
with TestAPI.Servers;
with TestBinary.Servers;
procedure TestAPI.Server (Server : in out Servlet.Server.Container'Class)is
   use Util.Properties.Basic;

   CONFIG_PATH  : constant String := "testapi.properties";

   Port : Natural := 8080;
   App     : aliased OpenAPI.Servers.Applications.Application_Type;
   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("TestAPI.Server");
   Props   : Util.Properties.Manager;
   Config  : Servlet.Server.Configuration;
begin
   Props.Load_Properties (CONFIG_PATH);
   Util.Log.Loggers.Initialize (Props);

   Port := Integer_Property.Get (Props, "swagger.port", Port);
   Config.Listening_Port := Port;
   App.Configure (Props);
   TestAPI.Servers.Server_Impl.Register (App);
   TestBinary.Servers.Server_Impl.Register (App);

   Server.Configure (Config);
   Server.Register_Application ("/v1", App'Unchecked_Access);
   App.Dump_Routes (Util.Log.INFO_LEVEL);
   Log.Info ("Connect you browser to: http://localhost:{0}/v1/ui/index.html",
             Util.Strings.Image (Port));

   Server.Start;

   delay 6000.0;

exception
   when Ada.IO_Exceptions.Name_Error =>
      Log.Error ("Cannot read application configuration file {0}", CONFIG_PATH);
end TestAPI.Server;
