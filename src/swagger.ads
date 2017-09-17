-----------------------------------------------------------------------
--  swagger -- Support library for swagger code generator
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
with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Strings.Hash;
with Ada.Iterator_Interfaces;
with Ada.Containers.Indefinite_Hashed_Maps;
with Util.Beans.Objects.Vectors;
with Util.Strings.Vectors;
package Swagger is

   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   function To_String (S : in UString) return String
     renames Ada.Strings.Unbounded.To_String;

   function TO_UString (S : in String) return UString
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   subtype Date is Ada.Calendar.Time;

   subtype Datetime is Ada.Calendar.Time;

   subtype Vector is Util.Beans.Objects.Vectors.Vector;

   subtype Long is Long_Long_Integer;

   subtype Http_Content_Type is String;

   subtype Number is Natural;

   subtype Object is Util.Beans.Objects.Object;
   subtype Value_Type is Util.Beans.Objects.Object;

   package UString_Vectors renames Util.Strings.Vectors;

   --  Convert the long value into a string.
   function To_String (Value : in Long) return String;

   package Integer_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                Element_Type    => Integer,
                                                Hash            => Ada.Strings.Hash,
                                                Equivalent_Keys => "=");

   subtype Integer_Map is Integer_Maps.Map;

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
      A : Natural;
   end record;

   type Value_Cursor is record
      Pos : Natural;
   end record;

   type Iterator is new Value_Iterator.Forward_Iterator with record
      N : Natural;
   end record;

   overriding
   function First (Iter : in Iterator) return Value_Cursor;

   overriding
   function Next (Object : in Iterator; Position : in Value_Cursor) return Value_Cursor;

end Swagger;
