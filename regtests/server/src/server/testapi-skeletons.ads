--  REST API Validation
--  API to validate
--
--  The version of the OpenAPI document: 1.0.0
--  Contact: Stephane.Carrez@gmail.com
--
--  NOTE: This package is auto generated by OpenAPI-Generator 7.0.0-SNAPSHOT.
--  https://openapi-generator.tech
--  Do not edit the class manually.

pragma Warnings (Off, "*is not referenced");
pragma Warnings (Off, "*no entities of*are referenced");
with Swagger.Servers;
with TestAPI.Models;
with Security.Permissions;
package TestAPI.Skeletons is
   pragma Style_Checks ("-bmrIu");
   pragma Warnings (Off, "*use clause for package*");
   use TestAPI.Models;
   type Server_Type is limited interface;

   --  Update a ticket
   package ACL_Write_Ticket is new Security.Permissions.Definition
     ("write:ticket");

   --  Read a ticket
   package ACL_Read_Ticket is new Security.Permissions.Definition
     ("read:ticket");

   --
   --  Query an orchestrated service instance
   procedure Orch_Store
     (Server                  : in out Server_Type;
      Orch_Store_Request_Type : in     OrchStoreRequest_Type;
      Context : in out Swagger.Servers.Context_Type) is abstract;

   --
   procedure Test_Int
     (Server  : in out Server_Type;
      Options : in     Swagger.Nullable_UString;
      Result  :    out TestAPI.Models.IntStruct_Type;
      Context : in out Swagger.Servers.Context_Type) is abstract;

   --
   procedure Test_Text_Response
     (Server  : in out Server_Type;
      Options : in     Swagger.Nullable_UString;
      Result  :    out Swagger.UString;
      Context : in out Swagger.Servers.Context_Type) is abstract;

   --  Create a ticket
   procedure Do_Create_Ticket
     (Server      : in out Server_Type;
      Title       : in     Swagger.UString;
      Owner       : in     Swagger.Nullable_UString;
      Status      : in     Swagger.Nullable_UString;
      Description : in     Swagger.Nullable_UString;
      Context     : in out Swagger.Servers.Context_Type) is abstract;

   --  Delete a ticket
   procedure Do_Delete_Ticket
     (Server  : in out Server_Type;
      Tid     : in     Swagger.Long;
      Context : in out Swagger.Servers.Context_Type) is abstract;

   --  List the tickets
   procedure Do_Head_Ticket
     (Server  : in out Server_Type;
      Context : in out Swagger.Servers.Context_Type) is abstract;

   --  Patch a ticket
   procedure Do_Patch_Ticket
     (Server      : in out Server_Type;
      Tid         : in     Swagger.Long;
      Owner       : in     Swagger.Nullable_UString;
      Status      : in     Swagger.Nullable_UString;
      Title       : in     Swagger.Nullable_UString;
      Description : in     Swagger.Nullable_UString;
      Result      :    out TestAPI.Models.Ticket_Type;
      Context     : in out Swagger.Servers.Context_Type) is abstract;

   --  Update a ticket
   procedure Do_Update_Ticket
     (Server      : in out Server_Type;
      Tid         : in     Swagger.Long;
      Owner       : in     Swagger.Nullable_UString;
      Status      : in     Swagger.Nullable_UString;
      Title       : in     Swagger.Nullable_UString;
      Description : in     Swagger.Nullable_UString;
      Result      :    out TestAPI.Models.Ticket_Type;
      Context     : in out Swagger.Servers.Context_Type) is abstract;

   --  Get a ticket
   --  Get a ticket
   procedure Do_Get_Ticket
     (Server  : in out Server_Type;
      Tid     : in     Swagger.Long;
      Result  :    out TestAPI.Models.Ticket_Type;
      Context : in out Swagger.Servers.Context_Type) is abstract;

   --  List the tickets
   --  List the tickets created for the project.
   procedure Do_List_Tickets
     (Server  : in out Server_Type;
      Status  : in     Swagger.Nullable_UString;
      Owner   : in     Swagger.Nullable_UString;
      Result  :    out TestAPI.Models.Ticket_Type_Vectors.Vector;
      Context : in out Swagger.Servers.Context_Type) is abstract;

   --  Get a ticket
   --  Get a ticket
   procedure Do_Options_Ticket
     (Server  : in out Server_Type;
      Tid     : in     Swagger.Long;
      Result  :    out TestAPI.Models.Ticket_Type;
      Context : in out Swagger.Servers.Context_Type) is abstract;

   generic
      type Implementation_Type is limited new Server_Type with private;
      URI_Prefix : String := "";
   package Skeleton is

      procedure Register
        (Server : in out Swagger.Servers.Application_Type'Class);

      --
      procedure Orch_Store
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --
      procedure Test_Int
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --
      procedure Test_Text_Response
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  Create a ticket
      procedure Do_Create_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  Delete a ticket
      procedure Do_Delete_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  List the tickets
      procedure Do_Head_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  Patch a ticket
      procedure Do_Patch_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  Update a ticket
      procedure Do_Update_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  Get a ticket
      procedure Do_Get_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  List the tickets
      procedure Do_List_Tickets
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  Get a ticket
      procedure Do_Options_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

   end Skeleton;

   generic
      type Implementation_Type is limited new Server_Type with private;
      URI_Prefix : String := "";
   package Shared_Instance is

      procedure Register
        (Server : in out Swagger.Servers.Application_Type'Class);

      --
      procedure Orch_Store
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --
      procedure Test_Int
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --
      procedure Test_Text_Response
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  Create a ticket
      procedure Do_Create_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  Delete a ticket
      procedure Do_Delete_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  List the tickets
      procedure Do_Head_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  Patch a ticket
      procedure Do_Patch_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  Update a ticket
      procedure Do_Update_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  Get a ticket
      procedure Do_Get_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  List the tickets
      procedure Do_List_Tickets
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

      --  Get a ticket
      procedure Do_Options_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type);

   private
      protected Server is

         --
         procedure Orch_Store
           (Orch_Store_Request_Type : in     OrchStoreRequest_Type;
            Context                 : in out Swagger.Servers.Context_Type);

         --
         procedure Test_Int
           (Options : in     Swagger.Nullable_UString;
            Result  :    out TestAPI.Models.IntStruct_Type;
            Context : in out Swagger.Servers.Context_Type);

         --
         procedure Test_Text_Response
           (Options : in     Swagger.Nullable_UString;
            Result  :    out Swagger.UString;
            Context : in out Swagger.Servers.Context_Type);

         --  Create a ticket
         procedure Do_Create_Ticket
           (Title       : in     Swagger.UString;
            Owner       : in     Swagger.Nullable_UString;
            Status      : in     Swagger.Nullable_UString;
            Description : in     Swagger.Nullable_UString;
            Context     : in out Swagger.Servers.Context_Type);

         --  Delete a ticket
         procedure Do_Delete_Ticket
           (Tid     : in     Swagger.Long;
            Context : in out Swagger.Servers.Context_Type);

         --  List the tickets
         procedure Do_Head_Ticket
           (Context : in out Swagger.Servers.Context_Type);

         --  Patch a ticket
         procedure Do_Patch_Ticket
           (Tid         : in     Swagger.Long;
            Owner       : in     Swagger.Nullable_UString;
            Status      : in     Swagger.Nullable_UString;
            Title       : in     Swagger.Nullable_UString;
            Description : in     Swagger.Nullable_UString;
            Result      :    out TestAPI.Models.Ticket_Type;
            Context     : in out Swagger.Servers.Context_Type);

         --  Update a ticket
         procedure Do_Update_Ticket
           (Tid         : in     Swagger.Long;
            Owner       : in     Swagger.Nullable_UString;
            Status      : in     Swagger.Nullable_UString;
            Title       : in     Swagger.Nullable_UString;
            Description : in     Swagger.Nullable_UString;
            Result      :    out TestAPI.Models.Ticket_Type;
            Context     : in out Swagger.Servers.Context_Type);

         --  Get a ticket
         procedure Do_Get_Ticket
           (Tid     : in     Swagger.Long;
            Result  :    out TestAPI.Models.Ticket_Type;
            Context : in out Swagger.Servers.Context_Type);

         --  List the tickets
         procedure Do_List_Tickets
           (Status  : in     Swagger.Nullable_UString;
            Owner   : in     Swagger.Nullable_UString;
            Result  :    out TestAPI.Models.Ticket_Type_Vectors.Vector;
            Context : in out Swagger.Servers.Context_Type);

         --  Get a ticket
         procedure Do_Options_Ticket
           (Tid     : in     Swagger.Long;
            Result  :    out TestAPI.Models.Ticket_Type;
            Context : in out Swagger.Servers.Context_Type);

      private
         Impl : Implementation_Type;
      end Server;
   end Shared_Instance;

end TestAPI.Skeletons;
