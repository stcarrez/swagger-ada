--  REST API Validation
--  API to validate
--
--  The version of the OpenAPI document: 1.0.0
--  Contact: Stephane.Carrez@gmail.com
--
--  NOTE: This package is auto generated by OpenAPI-Generator 6.1.0-SNAPSHOT.
--  https://openapi-generator.tech
--  Do not edit the class manually.

with Swagger.Streams;
with Ada.Containers.Vectors;
package TestBinary.Models is
   pragma Style_Checks ("-bmrIu");

   type Status_Type is (OPEN, ONHOLD, ASSIGNED, CLOSED, REJECTED);

   function To_Status_Type (Value : in String) return Status_Type;
   function To_String (Value : in Status_Type) return String;

   package Status_Type_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Status_Type);

   procedure Serialize
     (Into  : in out Swagger.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     Status_Type);

   procedure Serialize
     (Into  : in out Swagger.Streams.Output_Stream'Class;
      Name  : in     String;
      Value : in     Status_Type_Vectors.Vector);

   procedure Deserialize
     (From : in Swagger.Value_Type; Name : in String; Value : out Status_Type);

   procedure Deserialize
     (From  : in     Swagger.Value_Type;
      Name  : in     String;
      Value :    out Status_Type_Vectors.Vector);

end TestBinary.Models;