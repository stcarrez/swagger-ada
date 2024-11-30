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
with Util.Http;
package body Types.Servers is

   --
   overriding procedure Add_Rack
     (Server    : in out Server_Type;
      Rack_Info : in     RackInfo_Type;
      Result    :    out Types.Models.RackInfo_Type;
      Context   : in out OpenAPI.Servers.Context_Type)
   is
   begin
      Result := Rack_Info;
      Result.Id := Server.Next_Id;
      Server.Next_Id := Server.Next_Id + 1;
      Server.Racks.Insert (Result.Id, Result);
   end Add_Rack;

   --
   overriding procedure Get_Rack
     (Server  : in out Server_Type;
      Name    : in     OpenAPI.Long;
      Result  :    out Types.Models.RackInfo_Type;
      Context : in out OpenAPI.Servers.Context_Type)
   is
      Pos : Rack_Maps.Cursor := Server.Racks.Find (Name);
   begin
      if not Rack_Maps.Has_Element (Pos) then
         Context.Set_Status (Util.Http.SC_NOT_FOUND);
         return;
      end if;
      Result := Rack_Maps.Element (Pos);
   end Get_Rack;

   --
   overriding procedure Update_Rack
     (Server    : in out Server_Type;
      Name      : in     OpenAPI.Long;
      Rack_Info : in     RackInfo_Type;
      Result    :    out Types.Models.RackInfo_Type;
      Context   : in out OpenAPI.Servers.Context_Type)
   is
      Pos : Rack_Maps.Cursor := Server.Racks.Find (Name);
   begin
      if not Rack_Maps.Has_Element (Pos) then
         Context.Set_Status (Util.Http.SC_NOT_FOUND);
         return;
      end if;
      Result := Rack_Info;
      Result.Id := Name;
      Server.Racks.Reference (Pos) := Result;
   end Update_Rack;

end Types.Servers;
