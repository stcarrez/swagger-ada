--  REST API types validation
--  Types to validate
--  ------------ EDIT NOTE ------------
--  This file was generated with openapi-generator.  You can modify it to implement
--  the server.  After you modify this file, you should add the following line
--  to the .openapi-generator-ignore file:
--
--  src/types-servers.adb
--
--  Then, you can drop this edit note comment.
--  ------------ EDIT NOTE ------------
package body Types.Servers is

   --
   overriding procedure Add_Rack
     (Server    : in out Server_Type;
      Rack_Info : in     RackInfo_Type;
      Result    :    out Types.Models.RackInfo_Type;
      Context   : in out OpenAPI.Servers.Context_Type)
   is
   begin
      null;
   end Add_Rack;

   --
   overriding procedure Get_Rack
     (Server  : in out Server_Type;
      Name    : in     OpenAPI.Long;
      Result  :    out Types.Models.RackInfo_Type;
      Context : in out OpenAPI.Servers.Context_Type)
   is
   begin
      null;
   end Get_Rack;

   --
   overriding procedure Update_Rack
     (Server    : in out Server_Type;
      Name      : in     OpenAPI.Long;
      Rack_Info : in     RackInfo_Type;
      Result    :    out Types.Models.RackInfo_Type;
      Context   : in out OpenAPI.Servers.Context_Type)
   is
   begin
      null;
   end Update_Rack;

end Types.Servers;
