-----------------------------------------------------------------------
--  openapi -- Support library for OpenAPI code generator
--  Copyright (C) 2017, 2022 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Strings.Hash;
with Ada.Iterator_Interfaces;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Util.Beans.Objects.Vectors;
with Util.Beans.Objects.Maps;
with Util.Strings.Vectors;
with Util.Nullables;
with Util.Blobs;
with Util.Http.Mimes;

--  == OpenAPI Introduction ==
--  The OpenAPI Ada library provides a small runtime for use by the OpenAPI Codegen
--  REST API generator.  The library provides operations that are used by the generated
--  REST client and servers to:
--
--    * serialize and deserialize the data in JSON or XML,
--    * make the client REST operation and retrieve the result,
--    * let a server receive a REST operation, handle it and return the result
--
--  The <tt>Swagger</tt> root package defines the global types that are used by
--  the generator to represent values such as strings, integers, dates.
--
--  @include openapi-clients.ads
--  @include openapi-servers.ads
--  @include openapi-streams.ads
package OpenAPI is

   subtype Mime_List is Util.Http.Mimes.Mime_List;
   subtype Mime_List_Access is Util.Http.Mimes.Mime_List_Access;
   subtype Mime_Access is Util.Http.Mimes.Mime_Access;

   Mime_Json    : constant Mime_Access := Util.Http.Mimes.Json'Access;
   Mime_Text    : constant Mime_Access := Util.Http.Mimes.Text'Access;
   Mime_Xml     : constant Mime_Access := Util.Http.Mimes.Xml'Access;
   Mime_Form    : constant Mime_Access := Util.Http.Mimes.Form'Access;

   subtype UString is Ada.Strings.Unbounded.Unbounded_String;
   subtype Nullable_UString is Util.Nullables.Nullable_String;

   function To_String (S : in UString) return String
     renames Ada.Strings.Unbounded.To_String;

   function To_UString (S : in String) return UString
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   subtype Date is Ada.Calendar.Time;
   subtype Nullable_Date is Util.Nullables.Nullable_Time;

   subtype Datetime is Ada.Calendar.Time;

   subtype Vector is Util.Beans.Objects.Vectors.Vector;

   subtype Long is Long_Long_Integer;
   subtype Nullable_Long is Util.Nullables.Nullable_Long;
   subtype Nullable_Integer is Util.Nullables.Nullable_Integer;
   subtype Nullable_Boolean is Util.Nullables.Nullable_Boolean;

   subtype Http_Content_Type is UString;
   subtype Blob_Ref is Util.Blobs.Blob_Ref;
   subtype File_Part_Type is Util.Blobs.Blob_Ref;

   subtype Number is Natural;

   subtype Object is Util.Beans.Objects.Object;
   subtype Value_Type is Util.Beans.Objects.Object;

   package UString_Vectors renames Util.Strings.Vectors;

   subtype Object_Map is Util.Beans.Objects.Maps.Map;

   function Is_Null (Value : in Object) return Boolean
     renames Util.Beans.Objects.Is_Null;
   function To_String (Value : in Object) return String
     renames Util.Beans.Objects.To_String;

   package Nullable_UString_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Nullable_UString,
                                 "="          => Util.Nullables."=");

   --  Convert the long value into a string.
   function To_String (Value : in Long) return String;

   package Integer_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => Integer,
                                                Hash            => Ada.Strings.Hash,
                                                Equivalent_Keys => "=");

   subtype Integer_Map is Integer_Maps.Map;

   use Util.Nullables;
   package Nullable_Integer_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => Nullable_Integer,
                                                Hash            => Ada.Strings.Hash,
                                                Equivalent_Keys => "=");

   subtype Nullable_Integer_Map is Nullable_Integer_Maps.Map;

   type Value_Array_Type is tagged private
     with
       Constant_Indexing => Element_Value,
       Default_Iterator => Iterate,
       Iterator_Element => Value_Type;

   type Value_Cursor is private;

   function Has_Element (Pos : in Value_Cursor) return Boolean;

   package Value_Iterator is
     new Ada.Iterator_Interfaces (Cursor       => Value_Cursor,
                                  Has_Element  => Has_Element);

   function Element_Value (List : in Value_Array_Type;
                           Pos  : in Value_Cursor) return Value_Type;

   function Iterate (List : in Value_Array_Type) return Value_Iterator.Forward_Iterator'Class;

private

   type Value_Array_Type is tagged record
      A : Util.Beans.Objects.Object;
   end record;

   type Value_Cursor is record
      List : Util.Beans.Objects.Object;
      Pos  : Natural := 0;
   end record;

   type Iterator is new Value_Iterator.Forward_Iterator with record
      List : Util.Beans.Objects.Object;
      Pos  : Natural := 0;
   end record;

   overriding
   function First (Iter : in Iterator) return Value_Cursor;

   overriding
   function Next (Object : in Iterator; Position : in Value_Cursor) return Value_Cursor;

end OpenAPI;
