-----------------------------------------------------------------------
--  swagger-streams -- Stream operations
--  Copyright (C) 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Util.Serialize.IO;
package Swagger.Streams is

   subtype Output_Stream is Util.Serialize.IO.Output_Stream;

   type Output_Stream_Access is access all Output_Stream'Class;

   --  Serialize a list of strings in the stream.
   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in Swagger.UString_Vectors.Vector);

   --  Serialize a long value.
   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in Swagger.Long);

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out Boolean);

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out Integer);

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out Long);

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out UString);

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out Value_Type);

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out Value_Array_Type);

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out Ada.Calendar.Time);


end Swagger.Streams;
