-----------------------------------------------------------------------
--  openapi -- Support library for swagger code generator
--  Copyright (C) 2017, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body OpenAPI is

   --  ------------------------------
   --  Convert the long value into a string.
   --  ------------------------------
   function To_String (Value : in Long) return String is
      Result : constant String := Long'Image (Value);
   begin
      return
        (if Result (Result'First) = ' ' then
           Result (Result'First + 1 .. Result'Last)
         else Result);
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
