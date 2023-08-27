--  REST API Validation
--  API to validate
--
--  The version of the OpenAPI document: 1.0.0
--  Contact: Stephane.Carrez@gmail.com
--
--  NOTE: This package is auto generated by OpenAPI-Generator 7.0.1-2023-08-27.
--  https://openapi-generator.tech
--  Do not edit the class manually.

package body TestAPI.Models is
   pragma Style_Checks ("-bmrIu");

   pragma Warnings (Off, "*use clause for package*");

   use Swagger.Streams;

   procedure Serialize
     (Into  : in out Swagger.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     TestAPI.Models.IntStruct_Type)
   is
   begin
      Into.Start_Entity (Name);
      Into.Write_Long_Entity ("long_int", Value.Long_Int);
      Into.Write_Entity ("short_int", Value.Short_Int);
      Into.Write_Entity ("short_int2", Value.Short_Int_2);
      Serialize (Into, "float_a", Value.Float_A);
      Serialize (Into, "float_b", Value.Float_B);
      Into.End_Entity (Name);
   end Serialize;

   procedure Deserialize
     (From  : in     Swagger.Value_Type;
      Name  : in     String;
      Value :    out TestAPI.Models.IntStruct_Type)
   is
      Object : Swagger.Value_Type;
   begin
      Swagger.Streams.Deserialize (From, Name, Object);
      Swagger.Streams.Deserialize (Object, "long_int", Value.Long_Int);
      Swagger.Streams.Deserialize (Object, "short_int", Value.Short_Int);
      Swagger.Streams.Deserialize (Object, "short_int2", Value.Short_Int_2);
      Swagger.Streams.Deserialize (Object, "float_a", Value.Float_A);
      Swagger.Streams.Deserialize (Object, "float_b", Value.Float_B);
   end Deserialize;

   procedure Serialize
     (Into  : in out Swagger.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     TestAPI.Models.Options_Type)
   is
   begin
      Into.Start_Entity (Name);
      Serialize (Into, "a", Value.A);
      Into.End_Entity (Name);
   end Serialize;

   procedure Serialize
     (Into  : in out Swagger.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     Options_Type_Vectors.Vector)
   is
   begin
      Into.Start_Array (Name);
      for Item of Value loop
         Serialize (Into, "", Item);
      end loop;
      Into.End_Array (Name);
   end Serialize;

   procedure Deserialize
     (From  : in     Swagger.Value_Type;
      Name  : in     String;
      Value :    out TestAPI.Models.Options_Type)
   is
      Object : Swagger.Value_Type;
   begin
      Swagger.Streams.Deserialize (From, Name, Object);
      Swagger.Streams.Deserialize (Object, "a", Value.A);
   end Deserialize;

   procedure Deserialize
     (From  : in     Swagger.Value_Type;
      Name  : in     String;
      Value : in out Options_Type_Vectors.Vector)
   is
      List : Swagger.Value_Array_Type;
      Item : TestAPI.Models.Options_Type;
   begin
      Value.Clear;
      Swagger.Streams.Deserialize (From, Name, List);
      for Data of List loop
         Deserialize (Data, "", Item);
         Value.Append (Item);
      end loop;
   end Deserialize;

   procedure Serialize
     (Into  : in out Swagger.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     TestAPI.Models.StringsMap_Type)
   is
   begin
      Into.Start_Entity (Name);
      if not Value.Key.Is_Null then
         Into.Write_Entity ("key", Value.Key);
      end if;
      Into.End_Entity (Name);
   end Serialize;

   procedure Serialize
     (Into  : in out Swagger.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     StringsMap_Type_Vectors.Vector)
   is
   begin
      Into.Start_Array (Name);
      for Item of Value loop
         Serialize (Into, "", Item);
      end loop;
      Into.End_Array (Name);
   end Serialize;

   procedure Deserialize
     (From  : in     Swagger.Value_Type;
      Name  : in     String;
      Value :    out TestAPI.Models.StringsMap_Type)
   is
      Object : Swagger.Value_Type;
   begin
      Swagger.Streams.Deserialize (From, Name, Object);
      Swagger.Streams.Deserialize (Object, "key", Value.Key);
   end Deserialize;

   procedure Deserialize
     (From  : in     Swagger.Value_Type;
      Name  : in     String;
      Value : in out StringsMap_Type_Vectors.Vector)
   is
      List : Swagger.Value_Array_Type;
      Item : TestAPI.Models.StringsMap_Type;
   begin
      Value.Clear;
      Swagger.Streams.Deserialize (From, Name, List);
      for Data of List loop
         Deserialize (Data, "", Item);
         Value.Append (Item);
      end loop;
   end Deserialize;

   procedure Serialize
     (Into  : in out Swagger.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     TestAPI.Models.Ticket_Type)
   is
   begin
      Into.Start_Entity (Name);
      Into.Write_Long_Entity ("id", Value.Id);
      Into.Write_Entity ("title", Value.Title);
      Into.Write_Entity ("description", Value.Description);
      if not Value.Owner.Is_Null then
         Into.Write_Entity ("owner", Value.Owner);
      end if;
      Into.Write_Entity ("create_date", Value.Create_Date);
      if not Value.End_Date.Is_Null then
         Into.Write_Entity ("end_date", Value.End_Date);
      end if;
      if not Value.Update_Date.Is_Null then
         Into.Write_Entity ("update_date", Value.Update_Date);
      end if;
      Into.Write_Entity ("status", Value.Status);
      Into.End_Entity (Name);
   end Serialize;

   procedure Serialize
     (Into  : in out Swagger.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     Ticket_Type_Vectors.Vector)
   is
   begin
      Into.Start_Array (Name);
      for Item of Value loop
         Serialize (Into, "", Item);
      end loop;
      Into.End_Array (Name);
   end Serialize;

   procedure Deserialize
     (From  : in     Swagger.Value_Type;
      Name  : in     String;
      Value :    out TestAPI.Models.Ticket_Type)
   is
      Object : Swagger.Value_Type;
   begin
      Swagger.Streams.Deserialize (From, Name, Object);
      Swagger.Streams.Deserialize (Object, "id", Value.Id);
      Swagger.Streams.Deserialize (Object, "title", Value.Title);
      Swagger.Streams.Deserialize (Object, "description", Value.Description);
      Swagger.Streams.Deserialize (Object, "owner", Value.Owner);
      Swagger.Streams.Deserialize (Object, "create_date", Value.Create_Date);
      Swagger.Streams.Deserialize (Object, "end_date", Value.End_Date);
      Swagger.Streams.Deserialize (Object, "update_date", Value.Update_Date);
      Swagger.Streams.Deserialize (Object, "status", Value.Status);
   end Deserialize;

   procedure Deserialize
     (From  : in     Swagger.Value_Type;
      Name  : in     String;
      Value : in out Ticket_Type_Vectors.Vector)
   is
      List : Swagger.Value_Array_Type;
      Item : TestAPI.Models.Ticket_Type;
   begin
      Value.Clear;
      Swagger.Streams.Deserialize (From, Name, List);
      for Data of List loop
         Deserialize (Data, "", Item);
         Value.Append (Item);
      end loop;
   end Deserialize;

   procedure Serialize
     (Into  : in out Swagger.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     TestAPI.Models.OrchStoreRequest_Type)
   is
   begin
      Into.Start_Entity (Name);
      Serialize (Into, "requestedQoS", Value.Requested_Qo_S);
      Serialize (Into, "commands", Value.Commands);
      Into.End_Entity (Name);
   end Serialize;

   procedure Deserialize
     (From  : in     Swagger.Value_Type;
      Name  : in     String;
      Value :    out TestAPI.Models.OrchStoreRequest_Type)
   is
      Object : Swagger.Value_Type;
   begin
      Swagger.Streams.Deserialize (From, Name, Object);
      Deserialize (Object, "requestedQoS", Value.Requested_Qo_S);
      Deserialize (Object, "commands", Value.Commands);
   end Deserialize;

end TestAPI.Models;
