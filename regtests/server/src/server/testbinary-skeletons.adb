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
package body TestBinary.Skeletons is
   pragma Style_Checks ("-bmrIu");
   pragma Warnings (Off, "*use clause for package*");

   use Swagger.Streams;

   Mime_1       : aliased constant String            := "image/jpeg";
   Mime_2       : aliased constant String            := "image/png";
   Media_List_1 : aliased constant Swagger.Mime_List :=
     (1 => Mime_1'Access,
2       => Mime_2'Access);
   Media_List_2 : aliased constant Swagger.Mime_List :=
     (1 => Swagger.Mime_Json,
2       => Swagger.Mime_Xml);

   package body Skeleton is

      package API_Do_Get_Image is new Swagger.Servers.Operation
        (Handler => Do_Get_Image,
         Method  => Swagger.Servers.GET,
         URI     => URI_Prefix & "/binary",
         Mimes   => Media_List_1'Access);

      --  Get an image
      procedure Do_Get_Image
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Impl   : Implementation_Type;
         Status : Status_Type;
         Owner  : Swagger.Nullable_UString;
         Result : Swagger.Blob_Ref;
      begin

         Status :=
           To_Status_Type
             (Swagger.Servers.Get_Query_Parameter (Req, "status"));

         Swagger.Servers.Get_Query_Parameter (Req, "owner", Owner);

         Impl.Do_Get_Image (Status, Owner, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("successful operation");
            Swagger.Streams.Write (Stream, Result);

            return;
         end if;
         if Context.Get_Status = 404 then
            Context.Set_Description ("Invalid status value");
            return;
         end if;
      end Do_Get_Image;

      package API_Do_Get_Stats is new Swagger.Servers.Operation
        (Handler => Do_Get_Stats,
         Method  => Swagger.Servers.GET,
         URI     => URI_Prefix & "/external/{status}",
         Mimes   => Media_List_2'Access);

      --  Get some stat from external struct
      procedure Do_Get_Stats
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Impl   : Implementation_Type;
         Status : Status_Type;
         Result : External.Stat_Vector;
      begin

         Status :=
           To_Status_Type (Swagger.Servers.Get_Path_Parameter (Req, 1));

         Impl.Do_Get_Stats (Status, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("successful operation");

            Stream.Start_Document;
            TestBinary.Models.Serialize (Stream, "", Result);
            Stream.End_Document;

            return;
         end if;
         if Context.Get_Status = 404 then
            Context.Set_Description ("Invalid status value");
            return;
         end if;
      end Do_Get_Stats;

      procedure Register
        (Server : in out Swagger.Servers.Application_Type'Class)
      is
      begin
         Swagger.Servers.Register (Server, API_Do_Get_Image.Definition);
         Swagger.Servers.Register (Server, API_Do_Get_Stats.Definition);
      end Register;

   end Skeleton;

   package body Shared_Instance is

      --  Get an image
      procedure Do_Get_Image
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Status : Status_Type;
         Owner  : Swagger.Nullable_UString;
         Result : Swagger.Blob_Ref;
      begin
         Status :=
           To_Status_Type
             (Swagger.Servers.Get_Query_Parameter (Req, "status"));

         Swagger.Servers.Get_Query_Parameter (Req, "owner", Owner);

         Server.Do_Get_Image (Status, Owner, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("successful operation");
            Swagger.Streams.Write (Stream, Result);

            return;
         end if;
         if Context.Get_Status = 404 then
            Context.Set_Description ("Invalid status value");
            return;
         end if;
      end Do_Get_Image;

      package API_Do_Get_Image is new Swagger.Servers.Operation
        (Handler => Do_Get_Image,
         Method  => Swagger.Servers.GET,
         URI     => URI_Prefix & "/binary",
         Mimes   => Media_List_1'Access);

      --  Get some stat from external struct
      procedure Do_Get_Stats
        (Req     : in out Swagger.Servers.Request'Class;
         Reply   : in out Swagger.Servers.Response'Class;
         Stream  : in out Swagger.Servers.Output_Stream'Class;
         Context : in out Swagger.Servers.Context_Type)
      is
         Status : Status_Type;
         Result : External.Stat_Vector;
      begin

         Status :=
           To_Status_Type (Swagger.Servers.Get_Path_Parameter (Req, 1));

         Server.Do_Get_Stats (Status, Result, Context);
         if Context.Get_Status = 200 then
            Context.Set_Description ("successful operation");

            Stream.Start_Document;
            TestBinary.Models.Serialize (Stream, "", Result);
            Stream.End_Document;

            return;
         end if;
         if Context.Get_Status = 404 then
            Context.Set_Description ("Invalid status value");
            return;
         end if;
      end Do_Get_Stats;

      package API_Do_Get_Stats is new Swagger.Servers.Operation
        (Handler => Do_Get_Stats,
         Method  => Swagger.Servers.GET,
         URI     => URI_Prefix & "/external/{status}",
         Mimes   => Media_List_2'Access);

      procedure Register
        (Server : in out Swagger.Servers.Application_Type'Class)
      is
      begin
         Swagger.Servers.Register (Server, API_Do_Get_Image.Definition);
         Swagger.Servers.Register (Server, API_Do_Get_Stats.Definition);
      end Register;

      protected body Server is
         --  Get an image
         procedure Do_Get_Image
           (Status  : in     Status_Type;
            Owner   : in     Swagger.Nullable_UString;
            Result  :    out Swagger.Blob_Ref;
            Context : in out Swagger.Servers.Context_Type)
         is
         begin
            Impl.Do_Get_Image (Status, Owner, Result, Context);
         end Do_Get_Image;

         --  Get some stat from external struct
         procedure Do_Get_Stats
           (Status  : in     Status_Type;
            Result  :    out External.Stat_Vector;
            Context : in out Swagger.Servers.Context_Type)
         is
         begin
            Impl.Do_Get_Stats (Status, Result, Context);
         end Do_Get_Stats;

      end Server;

   end Shared_Instance;

end TestBinary.Skeletons;
