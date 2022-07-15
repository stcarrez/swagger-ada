-----------------------------------------------------------------------
--  openapi -- Support library for swagger code generator
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

package body OpenAPI is

   --  ------------------------------
   --  Convert the long value into a string.
   --  ------------------------------
   function To_String (Value : in Long) return String is
      Result : constant String := Long'Image (Value);
   begin
      if Result (Result'First) = ' ' then
         return Result (Result'First + 1 .. Result'Last);
      else
         return Result;
      end if;
   end To_String;

   function Has_Element (Pos : in Value_Cursor) return Boolean is
   begin
      return Pos.Pos > 0 and then Pos.Pos <= Util.Beans.Objects.Get_Count (Pos.List);
   end Has_Element;

   function Element_Value (List : in Value_Array_Type;
                           Pos  : in Value_Cursor) return Value_Type is
   begin
      return Util.Beans.Objects.Get_Value (List.A, Pos.Pos);
   end Element_Value;

   function Iterate (List : in Value_Array_Type) return Value_Iterator.Forward_Iterator'Class is
      Res : Iterator;
   begin
      Res.List := List.A;
      Res.Pos  := 1;
      return Res;
   end Iterate;

   overriding
   function First (Iter : in Iterator) return Value_Cursor is
      Res : Value_Cursor;
   begin
      Res.List := Iter.List;
      Res.Pos  := Iter.Pos;
      return Res;
   end First;

   overriding
   function Next (Object : in Iterator; Position : in Value_Cursor) return Value_Cursor is
      Res : Value_Cursor;
   begin
      Res.List := Object.List;
      Res.Pos  := Position.Pos + 1;
      return Res;
   end Next;

end OpenAPI;
