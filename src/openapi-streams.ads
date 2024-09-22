-----------------------------------------------------------------------
--  openapi-streams -- Stream operations
--  Copyright (C) 2017, 2020, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Blobs;
with Util.Serialize.IO;

--  == Streams ==
--  The <tt>Swagger.Streams</tt> package provides operations to manage data streams
--  in order to serialize and deserialize the data either in JSON or in XML.
package OpenAPI.Streams is

   subtype Output_Stream is Util.Serialize.IO.Output_Stream;

   type Output_Stream_Access is access all Output_Stream'Class;

   procedure Write (Stream : in out Output_Stream'Class;
                    Data   : in Util.Blobs.Blob_Ref);

   --  Serialize a list of strings in the stream.
   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.String_Vectors.Vector);

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.UString_Vectors.Vector);

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.Nullable_UString_Vectors.Vector);

   --  Serialize a long value.
   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.Long);
   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.Nullable_Long);

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.UString);

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.Nullable_UString);

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in Integer_Map);

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in Nullable_Integer_Map);

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in Object_Map);

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in Object_Vector);

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in Number);
   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.Number_Vectors.Vector);

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Boolean);

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Nullable_Boolean);

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Integer);
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Nullable_Integer);

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Long);
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Nullable_Long);

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out UString);

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Nullable_UString);

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Number);
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Number_Vectors.Vector);

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Value_Type);

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Value_Array_Type);

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Ada.Calendar.Time);

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Nullable_Date);

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out String_Vectors.Vector);

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out UString_Vectors.Vector);

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Nullable_UString_Vectors.Vector);

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Integer_Map);

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Nullable_Integer_Map);

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Object_Map);

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Object_Vector);

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Blob_Ref);

end OpenAPI.Streams;
