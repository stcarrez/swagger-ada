-----------------------------------------------------------------------
--  openapi-streams-forms -- x-www-form-urlencoded streams
--  Copyright (C) 2018, 2022 Stephane Carrez
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
with Ada.Streams;
with Util.Streams.Texts;
with Util.Serialize.IO;
package OpenAPI.Streams.Forms is

   type Output_Stream is limited new Util.Serialize.IO.Output_Stream with private;

   procedure Initialize (Stream : in out Output_Stream;
                         Buffer : in Util.Streams.Texts.Print_Stream_Access);

   --  Flush the buffer (if any) to the sink.
   overriding
   procedure Flush (Stream : in out Output_Stream);

   --  Close the sink.
   overriding
   procedure Close (Stream : in out Output_Stream);

   --  Write the buffer array to the output stream.
   overriding
   procedure Write (Stream : in out Output_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array);

   --  Start a document.
   overriding
   procedure Start_Document (Stream : in out Output_Stream) is null;

   --  Finish a document.
   overriding
   procedure End_Document (Stream : in out Output_Stream) is null;

   overriding
   procedure Start_Entity (Stream : in out Output_Stream;
                           Name   : in String) is null;

   overriding
   procedure End_Entity (Stream : in out Output_Stream;
                         Name   : in String) is null;

   --  Write the attribute name/value pair.
   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in String);

   overriding
   procedure Write_Wide_Attribute (Stream : in out Output_Stream;
                                   Name   : in String;
                                   Value  : in Wide_Wide_String);

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Integer);

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Boolean);

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object);

   --  Write the attribute with a null value.
   overriding
   procedure Write_Null_Attribute (Stream : in out Output_Stream;
                                   Name   : in String);

   --  Write the entity value.
   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in String);

   overriding
   procedure Write_Wide_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in Wide_Wide_String);

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Boolean);

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Integer);

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Ada.Calendar.Time);

   overriding
   procedure Write_Long_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in Long_Long_Integer);

   overriding
   procedure Write_Enum_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in String);

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object);

   --  Write an entity with a null value.
   overriding
   procedure Write_Null_Entity (Stream : in out Output_Stream;
                                Name   : in String);

private

   type Output_Stream is limited new Util.Serialize.IO.Output_Stream with record
      Stream    : Util.Streams.Texts.Print_Stream_Access;
      Has_Param : Boolean := False;
   end record;

end OpenAPI.Streams.Forms;
