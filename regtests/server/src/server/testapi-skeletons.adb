--  REST API Validation
--  API to validate
--
--  The version of the OpenAPI document: 1.0.0
--  Contact: Stephane.Carrez@gmail.com
--
--  NOTE: This package is auto generated by OpenAPI-Generator 7.0.1-2023-08-25.
--  https://openapi-generator.tech
--  Do not edit the class manually.

pragma Warnings (Off, "*is not referenced");
with Swagger.Streams;
with Swagger.Servers.Operation;
package body TestAPI.Skeletons is
   pragma Style_Checks ("-bmrIu");
   pragma Warnings (Off, "*use clause for package*");

   use Swagger.Streams;

   Media_List_1 : aliased constant Swagger.Mime_List :=
     (1 => Swagger.Mime_Json);
   Media_List_2 : aliased constant Swagger.Mime_List :=
     (1 => Swagger.Mime_Text);
   Media_List_3 : aliased constant Swagger.Mime_List :=
     (1 => Swagger.Mime_Form);

   package body Skeleton is

      package API_Orch_Store is new Swagger.Servers.Operation
        (Handler => Orch_Store,
         Method  => Swagger.Servers.POST,
         URI     => URI_Prefix & "/orchestration",
         Mimes   => null);

      --
      procedure Orch_Store
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Input                   : Swagger.Value_Type;
         Impl                    : Implementation_Type;
         Orch_Store_Request_Type : OrchStoreRequest_Type;
      begin

         Swagger.Servers.Read (Req, Media_List_1, Input);
         if Swagger.Is_Null (Input) then
            Context.Set_Error (415, "Invalid content");
            return;
         end if;

         TestAPI.Models.Deserialize
           (Input, "OrchStoreRequest_Type", Orch_Store_Request_Type);
         Impl.Orch_Store (Orch_Store_Request_Type, Context);

      end Orch_Store;

      package API_Test_Int is new Swagger.Servers.Operation
        (Handler => Test_Int,
         Method  => Swagger.Servers.GET,
         URI     => URI_Prefix & "/testInt",
         Mimes   => Media_List_1'Access);

      --
      procedure Test_Int
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Impl    : Implementation_Type;
         Options : Swagger.Nullable_UString;
         Result  : TestAPI.Models.IntStruct_Type;
      begin

         Swagger.Servers.Get_Query_Parameter (Req, "options", Options);

         Impl.Test_Int (Options, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("OK");

            Stream.Start_Document;
            TestAPI.Models.Serialize (Stream, "", Result);
            Stream.End_Document;

            return;
         end if;
      end Test_Int;

      package API_Test_Text_Response is new Swagger.Servers.Operation
        (Handler => Test_Text_Response,
         Method  => Swagger.Servers.GET,
         URI     => URI_Prefix & "/testTextResponse",
         Mimes   => Media_List_2'Access);

      --
      procedure Test_Text_Response
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Impl    : Implementation_Type;
         Options : Swagger.Nullable_UString;
         Result  : Swagger.UString;
      begin

         Swagger.Servers.Get_Query_Parameter (Req, "options", Options);

         Impl.Test_Text_Response (Options, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("OK");

            Stream.Write (Swagger.To_String (Result));

            return;
         end if;
      end Test_Text_Response;

      package API_Do_Create_Ticket is new Swagger.Servers.Operation
        (Handler => Do_Create_Ticket,
         Method  => Swagger.Servers.POST,
         URI     => URI_Prefix & "/tickets",
         Mimes   => null);

      --  Create a ticket
      procedure Do_Create_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Impl        : Implementation_Type;
         Title       : Swagger.UString;
         Owner       : Swagger.Nullable_UString;
         Status      : Swagger.Nullable_UString;
         Description : Swagger.Nullable_UString;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Write_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Swagger.Servers.Get_Parameter (Context, "owner", Owner);
         Swagger.Servers.Get_Parameter (Context, "status", Status);
         Swagger.Servers.Get_Parameter (Context, "title", Title);
         Swagger.Servers.Get_Parameter (Context, "description", Description);
         Impl.Do_Create_Ticket (Title, Owner, Status, Description, Context);

      end Do_Create_Ticket;

      package API_Do_Delete_Ticket is new Swagger.Servers.Operation
        (Handler => Do_Delete_Ticket,
         Method  => Swagger.Servers.DELETE,
         URI     => URI_Prefix & "/tickets/{tid}",
         Mimes   => null);

      --  Delete a ticket
      procedure Do_Delete_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Impl : Implementation_Type;
         Tid  : Swagger.Long;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Write_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Swagger.Servers.Get_Path_Parameter (Req, 1, Tid);

         Impl.Do_Delete_Ticket (Tid, Context);

      end Do_Delete_Ticket;

      package API_Do_Head_Ticket is new Swagger.Servers.Operation
        (Handler => Do_Head_Ticket,
         Method  => Swagger.Servers.HEAD,
         URI     => URI_Prefix & "/tickets",
         Mimes   => null);

      --  List the tickets
      procedure Do_Head_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Impl : Implementation_Type;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Read_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Impl.Do_Head_Ticket (Context);

      end Do_Head_Ticket;

      package API_Do_Patch_Ticket is new Swagger.Servers.Operation
        (Handler => Do_Patch_Ticket,
         Method  => Swagger.Servers.PATCH,
         URI     => URI_Prefix & "/tickets/{tid}",
         Mimes   => Media_List_1'Access);

      --  Patch a ticket
      procedure Do_Patch_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Impl        : Implementation_Type;
         Tid         : Swagger.Long;
         Owner       : Swagger.Nullable_UString;
         Status      : Swagger.Nullable_UString;
         Title       : Swagger.Nullable_UString;
         Description : Swagger.Nullable_UString;
         Result      : TestAPI.Models.Ticket_Type;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Write_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Swagger.Servers.Get_Path_Parameter (Req, 1, Tid);

         Swagger.Servers.Get_Parameter (Context, "owner", Owner);
         Swagger.Servers.Get_Parameter (Context, "status", Status);
         Swagger.Servers.Get_Parameter (Context, "title", Title);
         Swagger.Servers.Get_Parameter (Context, "description", Description);
         Impl.Do_Patch_Ticket
           (Tid, Owner, Status, Title, Description, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("successful operation");

            Stream.Start_Document;
            TestAPI.Models.Serialize (Stream, "", Result);
            Stream.End_Document;

            return;
         end if;
         if Context.Get_Status = 405 then
            Context.Set_Description ("Invalid input");
            return;
         end if;
      end Do_Patch_Ticket;

      package API_Do_Update_Ticket is new Swagger.Servers.Operation
        (Handler => Do_Update_Ticket,
         Method  => Swagger.Servers.PUT,
         URI     => URI_Prefix & "/tickets/{tid}",
         Mimes   => Media_List_1'Access);

      --  Update a ticket
      procedure Do_Update_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Impl        : Implementation_Type;
         Tid         : Swagger.Long;
         Owner       : Swagger.Nullable_UString;
         Status      : Swagger.Nullable_UString;
         Title       : Swagger.Nullable_UString;
         Description : Swagger.Nullable_UString;
         Result      : TestAPI.Models.Ticket_Type;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Write_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Swagger.Servers.Get_Path_Parameter (Req, 1, Tid);

         Swagger.Servers.Get_Parameter (Context, "owner", Owner);
         Swagger.Servers.Get_Parameter (Context, "status", Status);
         Swagger.Servers.Get_Parameter (Context, "title", Title);
         Swagger.Servers.Get_Parameter (Context, "description", Description);
         Impl.Do_Update_Ticket
           (Tid, Owner, Status, Title, Description, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("successful operation");

            Stream.Start_Document;
            TestAPI.Models.Serialize (Stream, "", Result);
            Stream.End_Document;

            return;
         end if;
         if Context.Get_Status = 405 then
            Context.Set_Description ("Invalid input");
            return;
         end if;
      end Do_Update_Ticket;

      package API_Do_Get_Ticket is new Swagger.Servers.Operation
        (Handler => Do_Get_Ticket,
         Method  => Swagger.Servers.GET,
         URI     => URI_Prefix & "/tickets/{tid}",
         Mimes   => Media_List_1'Access);

      --  Get a ticket
      procedure Do_Get_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Impl   : Implementation_Type;
         Tid    : Swagger.Long;
         Result : TestAPI.Models.Ticket_Type;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Read_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Swagger.Servers.Get_Path_Parameter (Req, 1, Tid);

         Impl.Do_Get_Ticket (Tid, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("successful operation");

            Stream.Start_Document;
            TestAPI.Models.Serialize (Stream, "", Result);
            Stream.End_Document;

            return;
         end if;
         if Context.Get_Status = 400 then
            Context.Set_Description ("Invalid ID supplied");
            return;
         end if;
         if Context.Get_Status = 404 then
            Context.Set_Description ("Ticket not found");
            return;
         end if;
      end Do_Get_Ticket;

      package API_Do_List_Tickets is new Swagger.Servers.Operation
        (Handler => Do_List_Tickets,
         Method  => Swagger.Servers.GET,
         URI     => URI_Prefix & "/tickets",
         Mimes   => Media_List_1'Access);

      --  List the tickets
      procedure Do_List_Tickets
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Impl   : Implementation_Type;
         Status : Swagger.Nullable_UString;
         Owner  : Swagger.Nullable_UString;
         Result : TestAPI.Models.Ticket_Type_Vectors.Vector;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Read_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Swagger.Servers.Get_Query_Parameter (Req, "status", Status);

         Swagger.Servers.Get_Query_Parameter (Req, "owner", Owner);

         Impl.Do_List_Tickets (Status, Owner, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("successful operation");

            Stream.Start_Document;
            TestAPI.Models.Serialize (Stream, "", Result);
            Stream.End_Document;

            return;
         end if;
         if Context.Get_Status = 400 then
            Context.Set_Description ("Invalid status value");
            return;
         end if;
      end Do_List_Tickets;

      package API_Do_Options_Ticket is new Swagger.Servers.Operation
        (Handler => Do_Options_Ticket,
         Method  => Swagger.Servers.OPTIONS,
         URI     => URI_Prefix & "/tickets/{tid}",
         Mimes   => Media_List_1'Access);

      --  Get a ticket
      procedure Do_Options_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Impl   : Implementation_Type;
         Tid    : Swagger.Long;
         Result : TestAPI.Models.Ticket_Type;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Read_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Swagger.Servers.Get_Path_Parameter (Req, 1, Tid);

         Impl.Do_Options_Ticket (Tid, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("successful operation");

            Stream.Start_Document;
            TestAPI.Models.Serialize (Stream, "", Result);
            Stream.End_Document;

            return;
         end if;
         if Context.Get_Status = 400 then
            Context.Set_Description ("Invalid ID supplied");
            return;
         end if;
         if Context.Get_Status = 404 then
            Context.Set_Description ("Ticket not found");
            return;
         end if;
      end Do_Options_Ticket;

      procedure Register
        (Server : in out Swagger.Servers.Application_Type'Class)
      is
      begin
         Swagger.Servers.Register (Server, API_Orch_Store.Definition);
         Swagger.Servers.Register (Server, API_Test_Int.Definition);
         Swagger.Servers.Register (Server, API_Test_Text_Response.Definition);
         Swagger.Servers.Register (Server, API_Do_Create_Ticket.Definition);
         Swagger.Servers.Register (Server, API_Do_Delete_Ticket.Definition);
         Swagger.Servers.Register (Server, API_Do_Head_Ticket.Definition);
         Swagger.Servers.Register (Server, API_Do_Patch_Ticket.Definition);
         Swagger.Servers.Register (Server, API_Do_Update_Ticket.Definition);
         Swagger.Servers.Register (Server, API_Do_Get_Ticket.Definition);
         Swagger.Servers.Register (Server, API_Do_List_Tickets.Definition);
         Swagger.Servers.Register (Server, API_Do_Options_Ticket.Definition);
      end Register;

   end Skeleton;

   package body Shared_Instance is

      --
      procedure Orch_Store
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Input                   : Swagger.Value_Type;
         Orch_Store_Request_Type : OrchStoreRequest_Type;
      begin

         Swagger.Servers.Read (Req, Media_List_1, Input);

         TestAPI.Models.Deserialize
           (Input, "OrchStoreRequest_Type", Orch_Store_Request_Type);
         Server.Orch_Store (Orch_Store_Request_Type, Context);

      end Orch_Store;

      package API_Orch_Store is new Swagger.Servers.Operation
        (Handler => Orch_Store,
         Method  => Swagger.Servers.POST,
         URI     => URI_Prefix & "/orchestration",
         Mimes   => null);

      --
      procedure Test_Int
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Options : Swagger.Nullable_UString;
         Result  : TestAPI.Models.IntStruct_Type;
      begin

         Swagger.Servers.Get_Query_Parameter (Req, "options", Options);

         Server.Test_Int (Options, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("OK");

            Stream.Start_Document;
            TestAPI.Models.Serialize (Stream, "", Result);
            Stream.End_Document;

            return;
         end if;
      end Test_Int;

      package API_Test_Int is new Swagger.Servers.Operation
        (Handler => Test_Int,
         Method  => Swagger.Servers.GET,
         URI     => URI_Prefix & "/testInt",
         Mimes   => Media_List_1'Access);

      --
      procedure Test_Text_Response
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Options : Swagger.Nullable_UString;
         Result  : Swagger.UString;
      begin

         Swagger.Servers.Get_Query_Parameter (Req, "options", Options);

         Server.Test_Text_Response (Options, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("OK");

            Stream.Write (Swagger.To_String (Result));

            return;
         end if;
      end Test_Text_Response;

      package API_Test_Text_Response is new Swagger.Servers.Operation
        (Handler => Test_Text_Response,
         Method  => Swagger.Servers.GET,
         URI     => URI_Prefix & "/testTextResponse",
         Mimes   => Media_List_2'Access);

      --  Create a ticket
      procedure Do_Create_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Title       : Swagger.UString;
         Owner       : Swagger.Nullable_UString;
         Status      : Swagger.Nullable_UString;
         Description : Swagger.Nullable_UString;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Write_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Swagger.Servers.Get_Parameter (Context, "owner", Owner);
         Swagger.Servers.Get_Parameter (Context, "status", Status);
         Swagger.Servers.Get_Parameter (Context, "title", Title);
         Swagger.Servers.Get_Parameter (Context, "description", Description);
         Server.Do_Create_Ticket (Title, Owner, Status, Description, Context);

      end Do_Create_Ticket;

      package API_Do_Create_Ticket is new Swagger.Servers.Operation
        (Handler => Do_Create_Ticket,
         Method  => Swagger.Servers.POST,
         URI     => URI_Prefix & "/tickets",
         Mimes   => null);

      --  Delete a ticket
      procedure Do_Delete_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Tid : Swagger.Long;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Write_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Swagger.Servers.Get_Path_Parameter (Req, 1, Tid);

         Server.Do_Delete_Ticket (Tid, Context);

      end Do_Delete_Ticket;

      package API_Do_Delete_Ticket is new Swagger.Servers.Operation
        (Handler => Do_Delete_Ticket,
         Method  => Swagger.Servers.DELETE,
         URI     => URI_Prefix & "/tickets/{tid}",
         Mimes   => null);

      --  List the tickets
      procedure Do_Head_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Read_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Server.Do_Head_Ticket (Context);

      end Do_Head_Ticket;

      package API_Do_Head_Ticket is new Swagger.Servers.Operation
        (Handler => Do_Head_Ticket,
         Method  => Swagger.Servers.HEAD,
         URI     => URI_Prefix & "/tickets",
         Mimes   => null);

      --  Patch a ticket
      procedure Do_Patch_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Tid         : Swagger.Long;
         Owner       : Swagger.Nullable_UString;
         Status      : Swagger.Nullable_UString;
         Title       : Swagger.Nullable_UString;
         Description : Swagger.Nullable_UString;
         Result      : TestAPI.Models.Ticket_Type;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Write_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Swagger.Servers.Get_Path_Parameter (Req, 1, Tid);

         Swagger.Servers.Get_Parameter (Context, "owner", Owner);
         Swagger.Servers.Get_Parameter (Context, "status", Status);
         Swagger.Servers.Get_Parameter (Context, "title", Title);
         Swagger.Servers.Get_Parameter (Context, "description", Description);
         Server.Do_Patch_Ticket
           (Tid, Owner, Status, Title, Description, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("successful operation");

            Stream.Start_Document;
            TestAPI.Models.Serialize (Stream, "", Result);
            Stream.End_Document;

            return;
         end if;
         if Context.Get_Status = 405 then
            Context.Set_Description ("Invalid input");
            return;
         end if;
      end Do_Patch_Ticket;

      package API_Do_Patch_Ticket is new Swagger.Servers.Operation
        (Handler => Do_Patch_Ticket,
         Method  => Swagger.Servers.PATCH,
         URI     => URI_Prefix & "/tickets/{tid}",
         Mimes   => Media_List_1'Access);

      --  Update a ticket
      procedure Do_Update_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Tid         : Swagger.Long;
         Owner       : Swagger.Nullable_UString;
         Status      : Swagger.Nullable_UString;
         Title       : Swagger.Nullable_UString;
         Description : Swagger.Nullable_UString;
         Result      : TestAPI.Models.Ticket_Type;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Write_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Swagger.Servers.Get_Path_Parameter (Req, 1, Tid);

         Swagger.Servers.Get_Parameter (Context, "owner", Owner);
         Swagger.Servers.Get_Parameter (Context, "status", Status);
         Swagger.Servers.Get_Parameter (Context, "title", Title);
         Swagger.Servers.Get_Parameter (Context, "description", Description);
         Server.Do_Update_Ticket
           (Tid, Owner, Status, Title, Description, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("successful operation");

            Stream.Start_Document;
            TestAPI.Models.Serialize (Stream, "", Result);
            Stream.End_Document;

            return;
         end if;
         if Context.Get_Status = 405 then
            Context.Set_Description ("Invalid input");
            return;
         end if;
      end Do_Update_Ticket;

      package API_Do_Update_Ticket is new Swagger.Servers.Operation
        (Handler => Do_Update_Ticket,
         Method  => Swagger.Servers.PUT,
         URI     => URI_Prefix & "/tickets/{tid}",
         Mimes   => Media_List_1'Access);

      --  Get a ticket
      procedure Do_Get_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Tid    : Swagger.Long;
         Result : TestAPI.Models.Ticket_Type;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Read_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Swagger.Servers.Get_Path_Parameter (Req, 1, Tid);

         Server.Do_Get_Ticket (Tid, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("successful operation");

            Stream.Start_Document;
            TestAPI.Models.Serialize (Stream, "", Result);
            Stream.End_Document;

            return;
         end if;
         if Context.Get_Status = 400 then
            Context.Set_Description ("Invalid ID supplied");
            return;
         end if;
         if Context.Get_Status = 404 then
            Context.Set_Description ("Ticket not found");
            return;
         end if;
      end Do_Get_Ticket;

      package API_Do_Get_Ticket is new Swagger.Servers.Operation
        (Handler => Do_Get_Ticket,
         Method  => Swagger.Servers.GET,
         URI     => URI_Prefix & "/tickets/{tid}",
         Mimes   => Media_List_1'Access);

      --  List the tickets
      procedure Do_List_Tickets
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Status : Swagger.Nullable_UString;
         Owner  : Swagger.Nullable_UString;
         Result : TestAPI.Models.Ticket_Type_Vectors.Vector;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Read_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Swagger.Servers.Get_Query_Parameter (Req, "status", Status);

         Swagger.Servers.Get_Query_Parameter (Req, "owner", Owner);

         Server.Do_List_Tickets (Status, Owner, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("successful operation");

            Stream.Start_Document;
            TestAPI.Models.Serialize (Stream, "", Result);
            Stream.End_Document;

            return;
         end if;
         if Context.Get_Status = 400 then
            Context.Set_Description ("Invalid status value");
            return;
         end if;
      end Do_List_Tickets;

      package API_Do_List_Tickets is new Swagger.Servers.Operation
        (Handler => Do_List_Tickets,
         Method  => Swagger.Servers.GET,
         URI     => URI_Prefix & "/tickets",
         Mimes   => Media_List_1'Access);

      --  Get a ticket
      procedure Do_Options_Ticket
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Tid    : Swagger.Long;
         Result : TestAPI.Models.Ticket_Type;
      begin
         if not Context.Is_Authenticated then
            Context.Set_Error (401, "Not authenticated");
            return;
         end if;
         if not Context.Has_Permission (ACL_Read_Ticket.Permission) then
            Context.Set_Error (403, "Permission denied");
            return;
         end if;

         Swagger.Servers.Get_Path_Parameter (Req, 1, Tid);

         Server.Do_Options_Ticket (Tid, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("successful operation");

            Stream.Start_Document;
            TestAPI.Models.Serialize (Stream, "", Result);
            Stream.End_Document;

            return;
         end if;
         if Context.Get_Status = 400 then
            Context.Set_Description ("Invalid ID supplied");
            return;
         end if;
         if Context.Get_Status = 404 then
            Context.Set_Description ("Ticket not found");
            return;
         end if;
      end Do_Options_Ticket;

      package API_Do_Options_Ticket is new Swagger.Servers.Operation
        (Handler => Do_Options_Ticket,
         Method  => Swagger.Servers.OPTIONS,
         URI     => URI_Prefix & "/tickets/{tid}",
         Mimes   => Media_List_1'Access);

      procedure Register
        (Server : in out Swagger.Servers.Application_Type'Class)
      is
      begin
         Swagger.Servers.Register (Server, API_Orch_Store.Definition);
         Swagger.Servers.Register (Server, API_Test_Int.Definition);
         Swagger.Servers.Register (Server, API_Test_Text_Response.Definition);
         Swagger.Servers.Register (Server, API_Do_Create_Ticket.Definition);
         Swagger.Servers.Register (Server, API_Do_Delete_Ticket.Definition);
         Swagger.Servers.Register (Server, API_Do_Head_Ticket.Definition);
         Swagger.Servers.Register (Server, API_Do_Patch_Ticket.Definition);
         Swagger.Servers.Register (Server, API_Do_Update_Ticket.Definition);
         Swagger.Servers.Register (Server, API_Do_Get_Ticket.Definition);
         Swagger.Servers.Register (Server, API_Do_List_Tickets.Definition);
         Swagger.Servers.Register (Server, API_Do_Options_Ticket.Definition);
      end Register;

      protected body Server is
         --
         procedure Orch_Store
           (Orch_Store_Request_Type : in     OrchStoreRequest_Type;
            Context                 : in out Swagger.Servers.Context_Type)
         is
         begin
            Impl.Orch_Store (Orch_Store_Request_Type, Context);
         end Orch_Store;

         --
         procedure Test_Int
           (Options : in     Swagger.Nullable_UString;
            Result  :    out TestAPI.Models.IntStruct_Type;
            Context : in out Swagger.Servers.Context_Type)
         is
         begin
            Impl.Test_Int (Options, Result, Context);
         end Test_Int;

         --
         procedure Test_Text_Response
           (Options : in     Swagger.Nullable_UString;
            Result  :    out Swagger.UString;
            Context : in out Swagger.Servers.Context_Type)
         is
         begin
            Impl.Test_Text_Response (Options, Result, Context);
         end Test_Text_Response;

         --  Create a ticket
         procedure Do_Create_Ticket
           (Title       : in     Swagger.UString;
            Owner       : in     Swagger.Nullable_UString;
            Status      : in     Swagger.Nullable_UString;
            Description : in     Swagger.Nullable_UString;
            Context     : in out Swagger.Servers.Context_Type)
         is
         begin
            Impl.Do_Create_Ticket (Title, Owner, Status, Description, Context);
         end Do_Create_Ticket;

         --  Delete a ticket
         procedure Do_Delete_Ticket
           (Tid     : in     Swagger.Long;
            Context : in out Swagger.Servers.Context_Type)
         is
         begin
            Impl.Do_Delete_Ticket (Tid, Context);
         end Do_Delete_Ticket;

         --  List the tickets
         procedure Do_Head_Ticket
           (Context : in out Swagger.Servers.Context_Type)
         is
         begin
            Impl.Do_Head_Ticket (Context);
         end Do_Head_Ticket;

         --  Patch a ticket
         procedure Do_Patch_Ticket
           (Tid         : in     Swagger.Long;
            Owner       : in     Swagger.Nullable_UString;
            Status      : in     Swagger.Nullable_UString;
            Title       : in     Swagger.Nullable_UString;
            Description : in     Swagger.Nullable_UString;
            Result      :    out TestAPI.Models.Ticket_Type;
            Context     : in out Swagger.Servers.Context_Type)
         is
         begin
            Impl.Do_Patch_Ticket
              (Tid, Owner, Status, Title, Description, Result, Context);
         end Do_Patch_Ticket;

         --  Update a ticket
         procedure Do_Update_Ticket
           (Tid         : in     Swagger.Long;
            Owner       : in     Swagger.Nullable_UString;
            Status      : in     Swagger.Nullable_UString;
            Title       : in     Swagger.Nullable_UString;
            Description : in     Swagger.Nullable_UString;
            Result      :    out TestAPI.Models.Ticket_Type;
            Context     : in out Swagger.Servers.Context_Type)
         is
         begin
            Impl.Do_Update_Ticket
              (Tid, Owner, Status, Title, Description, Result, Context);
         end Do_Update_Ticket;

         --  Get a ticket
         procedure Do_Get_Ticket
           (Tid     : in     Swagger.Long;
            Result  :    out TestAPI.Models.Ticket_Type;
            Context : in out Swagger.Servers.Context_Type)
         is
         begin
            Impl.Do_Get_Ticket (Tid, Result, Context);
         end Do_Get_Ticket;

         --  List the tickets
         procedure Do_List_Tickets
           (Status  : in     Swagger.Nullable_UString;
            Owner   : in     Swagger.Nullable_UString;
            Result  :    out TestAPI.Models.Ticket_Type_Vectors.Vector;
            Context : in out Swagger.Servers.Context_Type)
         is
         begin
            Impl.Do_List_Tickets (Status, Owner, Result, Context);
         end Do_List_Tickets;

         --  Get a ticket
         procedure Do_Options_Ticket
           (Tid     : in     Swagger.Long;
            Result  :    out TestAPI.Models.Ticket_Type;
            Context : in out Swagger.Servers.Context_Type)
         is
         begin
            Impl.Do_Options_Ticket (Tid, Result, Context);
         end Do_Options_Ticket;

      end Server;

   end Shared_Instance;

end TestAPI.Skeletons;
