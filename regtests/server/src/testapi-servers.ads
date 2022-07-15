--  REST API Validation
--  API to validate
--  ------------ EDIT NOTE ------------
--  This file was generated with swagger-codegen.  You can modify it to implement
--  the server.  After you modify this file, you should add the following line
--  to the .swagger-codegen-ignore file:
--
--  src/testapi-servers.ads
--
--  Then, you can drop this edit note comment.
--  ------------ EDIT NOTE ------------
with Swagger.Servers;
with TestAPI.Models;
with TestAPI.Skeletons;
package TestAPI.Servers is
   use TestAPI.Models;

   type Server_Type is limited new TestAPI.Skeletons.Server_Type with record
      Todos   : TestAPI.Models.Ticket_Type_Vectors.Vector;
      Last_Id : Swagger.Long := 0;
   end record;


   --  Create a ticket
   overriding
   procedure Do_Create_Ticket
     (Server : in out Server_Type;
      Title : in Swagger.UString;
      Owner : in Swagger.Nullable_UString;
      Status : in Swagger.Nullable_UString;
      Description : in Swagger.Nullable_UString;
      Context : in out Swagger.Servers.Context_Type);

   --  Delete a ticket
   overriding
   procedure Do_Delete_Ticket
      (Server : in out Server_Type;
       Tid : in Swagger.Long;
       Context : in out Swagger.Servers.Context_Type);

   --  Patch a ticket
   overriding
   procedure Do_Patch_Ticket
      (Server : in out Server_Type;
       Tid : in Swagger.Long;
       Owner : in Swagger.Nullable_UString;
       Status : in Swagger.Nullable_UString;
       Title : in Swagger.Nullable_UString;
       Description : in Swagger.Nullable_UString;
       Result  : out TestAPI.Models.Ticket_Type;
       Context : in out Swagger.Servers.Context_Type);

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
       Context : in out Swagger.Servers.Context_Type);

   --  Get a ticket
   --  Get a ticket
   overriding
   procedure Do_Get_Ticket
      (Server : in out Server_Type;
       Tid : in Swagger.Long;
       Result  : out TestAPI.Models.Ticket_Type;
       Context : in out Swagger.Servers.Context_Type);

   --  Options the tickets
   --  List the tickets created for the project.
   overriding
   procedure Do_Options_Ticket
      (Server : in out Server_Type;
       Tid : in Swagger.Long;
       Result  : out TestAPI.Models.Ticket_Type;
       Context : in out Swagger.Servers.Context_Type);

   --  List the tickets
   overriding
   procedure Do_Head_Ticket
      (Server : in out Server_Type;
       Context : in out Swagger.Servers.Context_Type);

   --  List the tickets
   --  List the tickets created for the project.
   overriding
   procedure Do_List_Tickets
      (Server : in out Server_Type;
       Status : in Swagger.Nullable_UString;
       Owner : in Swagger.Nullable_UString;
       Result  : out TestAPI.Models.Ticket_Type_Vectors.Vector;
       Context : in out Swagger.Servers.Context_Type);

   --
   --  Query an orchestrated service instance
   overriding
   procedure Orch_Store
      (Server : in out Server_Type;
       Orch_Store_Request_Type : in OrchStoreRequest_Type;
       Context : in out Swagger.Servers.Context_Type);

   --  Test text/plain response
   overriding
   procedure Test_Text_Response
      (Server : in out Server_Type;
       Options : in Swagger.Nullable_UString;
       Result  : out Swagger.UString;
       Context : in out Swagger.Servers.Context_Type);

   package Server_Impl is
      new TestAPI.Skeletons.Shared_Instance (Server_Type);

end TestAPI.Servers;
