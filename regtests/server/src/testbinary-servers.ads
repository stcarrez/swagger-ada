--  REST API Validation
--  API to validate
--  ------------ EDIT NOTE ------------
--  This file was generated with openapi-generator.  You can modify it to implement
--  the server.  After you modify this file, you should add the following line
--  to the .openapi-generator-ignore file:
--
--  src/testbinary-servers.ads
--
--  Then, you can drop this edit note comment.
--  ------------ EDIT NOTE ------------
with Swagger.Servers;
with TestBinary.Models;
with TestBinary.Skeletons;
with External;
package TestBinary.Servers is
   pragma Warnings (Off, "*use clause for package*");
   use TestBinary.Models;
   type Server_Type is limited new TestBinary.Skeletons.Server_Type with
   null record;

   --  Get an image
   --  Get an image
   overriding procedure Do_Get_Image
     (Server  : in out Server_Type;
      Status  : in     Status_Type;
      Owner   : in     Swagger.Nullable_UString;
      Result  :    out Swagger.Blob_Ref;
      Context : in out Swagger.Servers.Context_Type);

   --  Get some stat from external struct
   overriding procedure Do_Get_Stats
     (Server  : in out Server_Type;
      Status  : in     Status_Type;
      Result  :    out External.Stat_Vector;
      Context : in out Swagger.Servers.Context_Type);

   package Server_Impl is new TestBinary.Skeletons.Shared_Instance
     (Server_Type);

end TestBinary.Servers;
