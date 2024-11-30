--  REST API Validation
--  API to validate
--  ------------ EDIT NOTE ------------
--  This file was generated with openapi-generator.  You can modify it to implement
--  the server.  After you modify this file, you should add the following line
--  to the .openapi-generator-ignore file:
--
--  src/enums-servers.adb
--
--  Then, you can drop this edit note comment.
--  ------------ EDIT NOTE ------------
package body Enums.Servers is

   --  Get some stat from external struct
   overriding procedure Do_Get_Enums
     (Server  : in out Server_Type;
      Status  : in     Status_Type;
      Result  :    out Enums.Models.Stat_Type_Vectors.Vector;
      Context : in out OpenAPI.Servers.Context_Type)
   is
      Item : Enums.Models.Stat_Type;
   begin
      Item.Status := Status;
      Item.Mode := Enums.Models.OPTIMIZED;
      for I in 1 .. 5 loop
         Item.Count := OpenAPI.Long (I);
         Item.Value.Is_Null := not Item.Value.Is_Null;
         Item.Value.Value := Item.Value.Value * 2;
         Result.Append (Item);
      end loop;
   end Do_Get_Enums;

   --  Get an object
   --  Get an object
   overriding procedure Do_Get_Object
     (Server  : in out Server_Type;
      Status  : in     Status_Type;
      Owner   : in     OpenAPI.Nullable_UString;
      Result  :    out OpenAPI.Object;
      Context : in out OpenAPI.Servers.Context_Type)
   is
   begin
      null;
   end Do_Get_Object;

end Enums.Servers;
