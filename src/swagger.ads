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
with Util.Beans.Objects.Vectors;
with Util.Strings.Vectors;
package Swagger is

   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   subtype Date is Ada.Calendar.Time;

   subtype Datetime is Ada.Calendar.Time;

   subtype Vector is Util.Beans.Objects.Vectors.Vector;

   subtype Long is Long_Long_Integer;

   subtype Http_Content_Type is String;

   subtype Number is Natural;

   subtype Object is Util.Beans.Objects.Object;

   package UString_Vectors renames Util.Strings.Vectors;

   --  Convert the long value into a string.
   function To_String (Value : in Long) return String;

end Swagger;
