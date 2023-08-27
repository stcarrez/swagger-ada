--  REST API Validation
--  API to validate
--
--  The version of the OpenAPI document: 1.0.0
--  Contact: Stephane.Carrez@gmail.com
--
--  NOTE: This package is auto generated by OpenAPI-Generator 7.0.1-2023-08-27.
--  https://openapi-generator.tech
--  Do not edit the class manually.

package body TestBinary.Models is
   pragma Style_Checks ("-bmrIu");

   pragma Warnings (Off, "*use clause for package*");

   use Swagger.Streams;

   procedure Serialize
     (Into  : in out Swagger.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     External.Stat_Type)
   is
   begin
      Into.Start_Entity (Name);
      Into.Write_Entity ("count", Value.Count);
      Into.Write_Entity ("name", Value.Name);
      Into.End_Entity (Name);
   end Serialize;

   procedure Serialize
     (Into  : in out Swagger.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     External.Stat_Vector)
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
      Value :    out External.Stat_Type)
   is
      Object : Swagger.Value_Type;
   begin
      Swagger.Streams.Deserialize (From, Name, Object);
      Swagger.Streams.Deserialize (Object, "count", Value.Count);
      Swagger.Streams.Deserialize (Object, "name", Value.Name);
   end Deserialize;

   procedure Deserialize
     (From  : in     Swagger.Value_Type;
      Name  : in     String;
      Value : in out External.Stat_Vector)
   is
      List : Swagger.Value_Array_Type;
      Item : External.Stat_Type;
   begin
      Value.Clear;
      Swagger.Streams.Deserialize (From, Name, List);
      for Data of List loop
         Deserialize (Data, "", Item);
         Value.Append (Item);
      end loop;
   end Deserialize;

   function To_Status_Type
     (Value : in String) return TestBinary.Models.Status_Type
   is
   begin
      if Value = "open" then
         return OPEN;
      end if;
      if Value = "onhold" then
         return ONHOLD;
      end if;
      if Value = "assigned" then
         return ASSIGNED;
      end if;
      if Value = "closed" then
         return CLOSED;
      end if;
      if Value = "rejected" then
         return REJECTED;
      end if;
      raise Constraint_Error;
   end To_Status_Type;

   function To_String (Value : in TestBinary.Models.Status_Type) return String
   is
   begin
      case Value is
         when OPEN =>
            return "open";

         when ONHOLD =>
            return "onhold";

         when ASSIGNED =>
            return "assigned";

         when CLOSED =>
            return "closed";

         when REJECTED =>
            return "rejected";

      end case;
   end To_String;
   procedure Serialize
     (Into  : in out Swagger.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     TestBinary.Models.Status_Type)
   is
   begin
      Into.Write_Entity (Name, To_String (Value));
   end Serialize;

   procedure Serialize
     (Into  : in out Swagger.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     Status_Type_Vectors.Vector)
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
      Value :    out TestBinary.Models.Status_Type)
   is
      Object : Swagger.Value_Type;
   begin
      Swagger.Streams.Deserialize (From, Name, Object);
      Value := To_Status_Type (Swagger.To_String (Object));
   end Deserialize;

   procedure Deserialize
     (From  : in     Swagger.Value_Type;
      Name  : in     String;
      Value : in out Status_Type_Vectors.Vector)
   is
      List : Swagger.Value_Array_Type;
      Item : TestBinary.Models.Status_Type;
   begin
      Value.Clear;
      Swagger.Streams.Deserialize (From, Name, List);
      for Data of List loop
         Deserialize (Data, "", Item);
         Value.Append (Item);
      end loop;
   end Deserialize;

end TestBinary.Models;
