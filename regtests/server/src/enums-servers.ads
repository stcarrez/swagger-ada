--  REST API Validation
--  API to validate
--  ------------ EDIT NOTE ------------
--  This file was generated with openapi-generator.  You can modify it to implement
--  the server.  After you modify this file, you should add the following line
--  to the .openapi-generator-ignore file:
--
--  src/enums-servers.ads
--
--  Then, you can drop this edit note comment.
--  ------------ EDIT NOTE ------------
with OpenAPI.Servers;
with Enums.Models;
with Enums.Skeletons;

package Enums.Servers is
   pragma Warnings (Off, "*use clause for package*");
   use Enums.Models;
   type Server_Type is limited new Enums.Skeletons.Server_Type with
   null record;

   --  Get some stat from external struct
   overriding procedure Do_Get_Enums
     (Server  : in out Server_Type;
      Status  : in     Status_Type;
      Result  :    out Enums.Models.Stat_Type_Vectors.Vector;
      Context : in out OpenAPI.Servers.Context_Type);

   --  Get an object
   --  Get an object
   overriding procedure Do_Get_Object
     (Server  : in out Server_Type;
      Status  : in     Status_Type;
      Owner   : in     OpenAPI.Nullable_UString;
      Result  :    out OpenAPI.Object;
      Context : in out OpenAPI.Servers.Context_Type);

   package Server_Impl is new Enums.Skeletons.Shared_Instance (Server_Type);

end Enums.Servers;
