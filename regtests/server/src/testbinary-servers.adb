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
with Ada.Strings.Unbounded;
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

   --  Get some stat from external struct
   overriding procedure Do_Get_Stats
     (Server  : in out Server_Type;
      Status  : in     Status_Type;
      Result  :    out External.Stat_Vector;
      Context : in out Swagger.Servers.Context_Type)
   is
      use Ada.Strings.Unbounded;
      Count : constant Natural := (case Status is
                                      when Open => 1,
                                      when Assigned => 2,
                                      when others => 10);
      Prefix : constant String := Status_Type'Image (Status);
   begin
      for I in 1 .. Count loop
         declare
            Item : External.Stat_Type;
         begin
            Item.Count := I;
            Item.Name := To_Unbounded_String (Prefix & Natural'Image (I));
            Result.Append (Item);
         end;
      end loop;
   end Do_Get_Stats;

end TestBinary.Servers;
