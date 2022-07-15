--  REST API Validation
--  API to validate
--  ------------ EDIT NOTE ------------
--  This file was generated with swagger-codegen.  You can modify it to implement
--  the server.  After you modify this file, you should add the following line
--  to the .swagger-codegen-ignore file:
--
--  src/testapi-servers.adb
--
--  Then, you can drop this edit note comment.
--  ------------ EDIT NOTE ------------
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Util.Strings;
package body TestAPI.Servers is


   --  Create a ticket
   overriding
   procedure Do_Create_Ticket
     (Server : in out Server_Type;
      Title : in Swagger.UString;
      Owner : in Swagger.Nullable_UString;
      Status : in Swagger.Nullable_UString;
      Description : in Swagger.Nullable_UString;
      Context : in out Swagger.Servers.Context_Type) is
      Id : constant Swagger.Long := Server.Last_Id + 1;
      T  : TestAPI.Models.Ticket_Type;
   begin
      Server.Last_Id := Id;
      T.Id := Id;
      T.Title := Title;
      if not Description.Is_Null then
         T.Description := Description.Value;
      end if;
      T.Create_Date := Ada.Calendar.Clock;
      T.Status := Swagger.To_UString ("open");
      Server.Todos.Append (T);
--        Context.Set_Location ("/tickets/{id}", Id);
      Context.Set_Location ("/tickets/" & Util.Strings.Image (Integer (Id)));
      Context.Set_Status (201);
   end Do_Create_Ticket;

   --  Delete a ticket
   overriding
   procedure Do_Delete_Ticket
      (Server : in out Server_Type;
       Tid : in Swagger.Long;
       Context : in out Swagger.Servers.Context_Type) is
      Pos : Models.Ticket_Type_Vectors.Cursor := Server.Todos.First;
   begin
      while Models.Ticket_Type_Vectors.Has_Element (Pos) loop
         if Models.Ticket_Type_Vectors.Element (Pos).Id = Tid then
            Server.Todos.Delete (Pos);
            Context.Set_Status (204);
            return;
         end if;
         Models.Ticket_Type_Vectors.Next (Pos);
      end loop;
      Context.Set_Error (404, "Ticket does not exist");
   end Do_Delete_Ticket;

   --  Update a ticket
   overriding
   procedure Do_Update_Ticket
      (Server : in out Server_Type;
       Tid : in Swagger.Long;
       Owner : in Swagger.Nullable_UString;
       Status : in Swagger.Nullable_UString;
       Title : in Swagger.Nullable_UString;
       Description : in Swagger.Nullable_UString;
       Result  : out TestAPI.Models.Ticket_Type;
       Context : in out Swagger.Servers.Context_Type) is
      procedure Update (T : in out Models.Ticket_Type) is
      begin
         if not Title.Is_Null then
            T.Title := Title.Value;
         end if;
         if not Description.Is_Null then
            T.Description := Description.Value;
         end if;
         if not Status.Is_Null then
            T.Status := Status.Value;
         end if;
      end Update;
      Pos : Models.Ticket_Type_Vectors.Cursor := Server.Todos.First;
   begin
      while Models.Ticket_Type_Vectors.Has_Element (Pos) loop
         if Models.Ticket_Type_Vectors.Element (Pos).Id = Tid then
            Server.Todos.Update_Element (Pos, Update'Access);
            Result := Models.Ticket_Type_Vectors.Element (Pos);
            return;
         end if;
         Models.Ticket_Type_Vectors.Next (Pos);
      end loop;
      Context.Set_Error (404, "Ticket does not exist");
   end Do_Update_Ticket;

   --  Patch a ticket
   procedure Do_Patch_Ticket
      (Server : in out Server_Type;
       Tid : in Swagger.Long;
       Owner : in Swagger.Nullable_UString;
       Status : in Swagger.Nullable_UString;
       Title : in Swagger.Nullable_UString;
       Description : in Swagger.Nullable_UString;
       Result  : out TestAPI.Models.Ticket_Type;
       Context : in out Swagger.Servers.Context_Type) is
      procedure Update (T : in out Models.Ticket_Type) is
      begin
         if not Title.Is_Null then
            T.Title := Title.Value;
         end if;
         if not Description.Is_Null then
            T.Description := Description.Value;
         end if;
         if not Status.Is_Null then
            T.Status := Status.Value;
         end if;
      end Update;
      Pos : Models.Ticket_Type_Vectors.Cursor := Server.Todos.First;
   begin
      while Models.Ticket_Type_Vectors.Has_Element (Pos) loop
         if Models.Ticket_Type_Vectors.Element (Pos).Id = Tid then
            Server.Todos.Update_Element (Pos, Update'Access);
            Result := Models.Ticket_Type_Vectors.Element (Pos);
            return;
         end if;
         Models.Ticket_Type_Vectors.Next (Pos);
      end loop;
      Context.Set_Error (404, "Ticket does not exist");
   end Do_Patch_Ticket;

   --  Get a ticket
   --  Get a ticket
   overriding
   procedure Do_Get_Ticket
      (Server : in out Server_Type;
       Tid : in Swagger.Long;
       Result  : out TestAPI.Models.Ticket_Type;
       Context : in out Swagger.Servers.Context_Type) is
   begin
      for T of Server.Todos loop
         if T.Id = Tid then
            Result := T;
            return;
         end if;
      end loop;
      Context.Set_Error (404, "Ticket does not exist");
   end Do_Get_Ticket;

   --  Get a ticket
   --  Get a ticket
   overriding
   procedure Do_Options_Ticket
      (Server : in out Server_Type;
       Tid : in Swagger.Long;
       Result  : out TestAPI.Models.Ticket_Type;
       Context : in out Swagger.Servers.Context_Type) is
   begin
      for T of Server.Todos loop
         if T.Id = Tid then
            Result := T;
            return;
         end if;
      end loop;
      Context.Set_Error (404, "Ticket does not exist");
   end Do_Options_Ticket;

   --  List the tickets
   overriding
   procedure Do_Head_Ticket
      (Server : in out Server_Type;
       Context : in out Swagger.Servers.Context_Type) is
   begin
      null;
   end Do_Head_Ticket;

   --  List the tickets
   --  List the tickets created for the project.
   overriding
   procedure Do_List_Tickets
      (Server : in out Server_Type;
       Status : in Swagger.Nullable_UString;
       Owner : in Swagger.Nullable_UString;
       Result  : out TestAPI.Models.Ticket_Type_Vectors.Vector;
       Context : in out Swagger.Servers.Context_Type) is
   begin
      Result := Server.Todos;
   end Do_List_Tickets;

   --
   --  Query an orchestrated service instance
   overriding
   procedure Orch_Store
      (Server : in out Server_Type;
       Orch_Store_Request_Type : in OrchStoreRequest_Type;
       Context : in out Swagger.Servers.Context_Type) is
   begin
      null;
   end Orch_Store;

   overriding
   procedure Test_Text_Response
      (Server : in out Server_Type;
       Options : in Swagger.Nullable_UString;
       Result  : out Swagger.UString;
       Context : in out Swagger.Servers.Context_Type) is
       use Ada.Strings.Unbounded;
   begin
       Result := Swagger.To_UString ("text response: ");
       if not Options.Is_Null then
          Append (Result, Options.Value);
       end if;
   end Test_Text_Response;

end TestAPI.Servers;
