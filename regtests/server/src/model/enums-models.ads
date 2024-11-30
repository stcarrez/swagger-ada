--  REST API Validation
--  API to validate
--
--  The version of the OpenAPI document: 1.0.0
--  Contact: Stephane.Carrez@gmail.com
--
--  NOTE: This package is auto generated by OpenAPI-Generator 7.11.0-2024-11-24.
--  https://openapi-generator.tech
--  Do not edit the class manually.

with OpenAPI.Streams;
with Ada.Containers.Vectors;
package Enums.Models is
   pragma Style_Checks ("-bmrIu");

   type Mode_Type is (FAST, OPTIMIZED, ACCURATE);

   function To_Mode_Type (Value : in String) return Enums.Models.Mode_Type;

   function To_String (Value : in Mode_Type) return String;

   procedure Serialize
     (Into  : in out OpenAPI.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     Enums.Models.Mode_Type);
   procedure Deserialize
     (From  : in     OpenAPI.Value_Type;
      Name  : in     String;
      Value :    out Enums.Models.Mode_Type);

   type Status_Type is (OPEN, ONHOLD, ASSIGNED, CLOSED, REJECTED);

   function To_Status_Type (Value : in String) return Enums.Models.Status_Type;

   function To_String (Value : in Status_Type) return String;

   package Status_Type_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Enums.Models.Status_Type);

   procedure Serialize
     (Into  : in out OpenAPI.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     Enums.Models.Status_Type);
   procedure Serialize
     (Into  : in out OpenAPI.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     Status_Type_Vectors.Vector);
   procedure Deserialize
     (From  : in     OpenAPI.Value_Type;
      Name  : in     String;
      Value :    out Enums.Models.Status_Type);
   procedure Deserialize
     (From  : in     OpenAPI.Value_Type;
      Name  : in     String;
      Value : in out Status_Type_Vectors.Vector);

   type Stat_Type is record
      Count  : OpenAPI.Long          := 300;
      Value  : OpenAPI.Nullable_Long := (Is_Null => False, Value => 500);
      Name   : OpenAPI.UString       := OpenAPI.To_UString ("plop");
      Mode   : Enums.Models.Mode_Type;
      Status : Enums.Models.Status_Type;
   end record;

   package Stat_Type_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Enums.Models.Stat_Type);

   procedure Serialize
     (Into  : in out OpenAPI.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     Enums.Models.Stat_Type);
   procedure Serialize
     (Into  : in out OpenAPI.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     Stat_Type_Vectors.Vector);
   procedure Deserialize
     (From  : in     OpenAPI.Value_Type;
      Name  : in     String;
      Value :    out Enums.Models.Stat_Type);
   procedure Deserialize
     (From  : in     OpenAPI.Value_Type;
      Name  : in     String;
      Value : in out Stat_Type_Vectors.Vector);

end Enums.Models;