--  REST API Validation
--  API to validate
--  ------------ EDIT NOTE ------------
--  This file was generated with openapi-generator.  You can modify it to implement
--  the server.  After you modify this file, you should add the following line
--  to the .openapi-generator-ignore file:
--
--  src/testbinary-servers.adb
--
--  Then, you can drop this edit note comment.
--  ------------ EDIT NOTE ------------
with Util.Blobs;
package body TestBinary.Servers is

   --  Get an image
   --  Get an image
   overriding procedure Do_Get_Image
     (Server  : in out Server_Type;
      Status  : in     Status_Type;
      Owner   : in     Swagger.Nullable_UString;
      Result  :    out Swagger.Blob_Ref;
      Context : in out Swagger.Servers.Context_Type)
   is
   begin
      case Status is
         when Open =>
            Result := Util.Blobs.Create_Blob (10);
            Result.Value.Data := (others => 1);

         when Onhold =>
            Result := Util.Blobs.Create_Blob (20);
            Result.Value.Data := (others => 3);

         when Assigned =>
            Result := Util.Blobs.Create_Blob (30);
            Result.Value.Data := (others => 5);

         when Closed =>
            Result := Util.Blobs.Create_Blob (40);
            Result.Value.Data := (others => 7);

         when Rejected =>
            Result := Util.Blobs.Create_Blob (50);
            Result.Value.Data := (others => 10);

      end case;
   end Do_Get_Image;

end TestBinary.Servers;
