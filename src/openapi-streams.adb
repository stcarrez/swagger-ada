-----------------------------------------------------------------------
--  openapi-streams -- Stream operations
--  Copyright (C) 2017, 2020, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Util.Beans.Objects.Maps;
with Util.Dates.ISO8601;
package body OpenAPI.Streams is

   procedure Write (Stream : in out Output_Stream'Class;
                    Data   : in Util.Blobs.Blob_Ref) is
   begin
      Stream.Write (Data.Value.Data);
   end Write;

   --  ------------------------------
   --  Serialize a list of strings in the stream.
   --  ------------------------------
   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.String_Vectors.Vector) is
   begin
      Stream.Start_Array (Name);
      for S of Value loop
         Stream.Write_Entity ("", S);
      end loop;
      Stream.End_Array (Name);
   end Serialize;

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.UString_Vectors.Vector) is
   begin
      Stream.Start_Array (Name);
      for S of Value loop
         Stream.Write_Entity ("", S);
      end loop;
      Stream.End_Array (Name);
   end Serialize;

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.Nullable_UString_Vectors.Vector) is
   begin
      Stream.Start_Array (Name);
      for S of Value loop
         Stream.Write_Entity ("", S);
      end loop;
      Stream.End_Array (Name);
   end Serialize;

   --  ------------------------------
   --  Serialize a long value.
   --  ------------------------------
   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.Long) is
   begin
      Stream.Write_Long_Entity (Name, Value);
   end Serialize;
   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.Nullable_Long) is
   begin
      if Value.Is_Null then
         Stream.Write_Null_Entity (Name);
      else
         Stream.Write_Long_Entity (Name, Value.Value);
      end if;
   end Serialize;

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.UString) is
   begin
      Stream.Write_Entity (Name, Value);
   end Serialize;

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.Nullable_UString) is
   begin
      Stream.Write_Entity (Name, Value);
   end Serialize;

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in Integer_Map) is
   begin
      null;
   end Serialize;

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in Nullable_Integer_Map) is
   begin
      null;
   end Serialize;

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in Object_Map) is
      procedure Process (Pos : in Util.Beans.Objects.Maps.Cursor);
      procedure Process (Pos : in Util.Beans.Objects.Maps.Cursor) is
         use Util.Beans.Objects.Maps;
      begin
         Stream.Write_Entity (Maps.Key (Pos), Maps.Element (Pos));
      end Process;
   begin
      if Name'Length > 0 then
         Stream.Start_Entity (Name);
      end if;
      Util.Beans.Objects.Maps.Maps.Iterate (Value, Process'Access);
      if Name'Length > 0 then
         Stream.End_Entity (Name);
      end if;
   end Serialize;

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in Object_Vector) is
   begin
      Stream.Start_Array (Name);
      for S of Value loop
         Stream.Write_Entity ("", S);
      end loop;
      Stream.End_Array (Name);
   end Serialize;

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.Number) is
   begin
      Stream.Write_Long_Entity (Name, Value);
   end Serialize;

   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in OpenAPI.Number_Vectors.Vector) is
   begin
      Stream.Start_Array (Name);
      for S of Value loop
         Stream.Write_Long_Entity ("", S);
      end loop;
      Stream.End_Array (Name);
   end Serialize;

   --  ------------------------------
   --  Extract a boolean value stored under the given name.
   --  ------------------------------
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Boolean) is
      Item : OpenAPI.Value_Type;
   begin
      if Name = "" then
         Item := From;
      else
         Deserialize (From, Name, Item);
      end if;
      Value := Util.Beans.Objects.To_Boolean (Item);
   end Deserialize;

   --  ------------------------------
   --  Extract a boolean value stored under the given name.
   --  ------------------------------
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Nullable_Boolean) is
      Item : OpenAPI.Value_Type;
   begin
      if Name = "" then
         Item := From;
      else
         Deserialize (From, Name, Item);
      end if;
      Value :=
        (if Util.Beans.Objects.Is_Null (Item) then
           (Is_Null => True, Value => <>)
         else (Is_Null => False,
            Value      => Util.Beans.Objects.To_Boolean (Item)));
   end Deserialize;

   --  ------------------------------
   --  Extract an integer value stored under the given name.
   --  ------------------------------
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Integer) is
      Item : OpenAPI.Value_Type;
   begin
      if Name = "" then
         Item := From;
      else
         Deserialize (From, Name, Item);
      end if;
      Value := Util.Beans.Objects.To_Integer (Item);
   end Deserialize;

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Nullable_Integer) is
      Item : OpenAPI.Value_Type;
   begin
      if Name = "" then
         Item := From;
      else
         Deserialize (From, Name, Item);
      end if;
      Value :=
        (if Util.Beans.Objects.Is_Null (Item) then
           (Is_Null => True, Value => 0)
         else (Is_Null => False,
            Value      => Util.Beans.Objects.To_Integer (Item)));
   end Deserialize;

   --  ------------------------------
   --  Extract an integer value stored under the given name.
   --  ------------------------------
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Long) is
      Item : OpenAPI.Value_Type;
   begin
      if Name = "" then
         Item := From;
      else
         Deserialize (From, Name, Item);
      end if;
      Value := Util.Beans.Objects.To_Long_Long_Integer (Item);
   end Deserialize;

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Nullable_Long) is
      Item : OpenAPI.Value_Type;
   begin
      if Name = "" then
         Item := From;
      else
         Deserialize (From, Name, Item);
      end if;
      Value :=
        (if Util.Beans.Objects.Is_Null (Item) then
           (Is_Null => True, Value => 0)
         else (Is_Null => False,
            Value      => Util.Beans.Objects.To_Long_Long_Integer (Item)));
   end Deserialize;

   --  ------------------------------
   --  Extract a string value stored under the given name.
   --  ------------------------------
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out UString) is
      Item : OpenAPI.Value_Type;
   begin
      if Name = "" then
         Item := From;
      else
         Deserialize (From, Name, Item);
      end if;
      Value := Util.Beans.Objects.To_Unbounded_String (Item);
   end Deserialize;

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Nullable_UString) is
      Item : OpenAPI.Value_Type;
   begin
      if Name = "" then
         Item := From;
      else
         Deserialize (From, Name, Item);
      end if;
      Value.Is_Null := Util.Beans.Objects.Is_Null (Item);
      if not Value.Is_Null then
         Value.Value := Util.Beans.Objects.To_Unbounded_String (Item);
      end if;
   end Deserialize;

   --  ------------------------------
   --  Extract an integer value stored under the given name.
   --  ------------------------------
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Long_Long_Float) is
      Item : OpenAPI.Value_Type;
   begin
      if Name = "" then
         Item := From;
      else
         Deserialize (From, Name, Item);
      end if;
      Value := Util.Beans.Objects.To_Long_Long_Float (Item);
   end Deserialize;

   --  ------------------------------
   --  Extract a value stored under the given name.
   --  ------------------------------
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Value_Type) is
   begin
      Value :=
        (if Name'Length = 0 then From
         else Util.Beans.Objects.Get_Value (From, Name));
   end Deserialize;

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Value_Array_Type) is
      List : Util.Beans.Objects.Object;
   begin
      List :=
        (if Name'Length = 0 then From
         else Util.Beans.Objects.Get_Value (From, Name));
      Value.A :=
        (if Util.Beans.Objects.Is_Array (List) then List
         else Util.Beans.Objects.Null_Object);
   end Deserialize;

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Ada.Calendar.Time) is
      Time : OpenAPI.Value_Type;
   begin
      Time :=
        (if Name'Length = 0 then From
         else Util.Beans.Objects.Get_Value (From, Name));
      declare
         T : constant String := Util.Beans.Objects.To_String (Time);
      begin
         Value := Util.Dates.ISO8601.Value (T);
      end;
   end Deserialize;

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Nullable_Date) is
      Time : OpenAPI.Value_Type;
   begin
      Time :=
        (if Name'Length = 0 then From
         else Util.Beans.Objects.Get_Value (From, Name));
      Value.Is_Null := Util.Beans.Objects.Is_Null (Time);
      if not Value.Is_Null then
         declare
            T : constant String := Util.Beans.Objects.To_String (Time);
         begin
            Value.Value := Util.Dates.ISO8601.Value (T);
         end;
      end if;
   end Deserialize;

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out String_Vectors.Vector) is
      use Util.Beans.Objects;
      List : Util.Beans.Objects.Object;
   begin
      List :=
        (if Name'Length = 0 then From
         else Util.Beans.Objects.Get_Value (From, Name));
      Value.Clear;
      if Is_Array (List) then
         for I in 1 .. Get_Count (List) loop
            Value.Append (To_String (Get_Value (List, I)));
         end loop;
      end if;
   end Deserialize;

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out UString_Vectors.Vector) is
      use Util.Beans.Objects;
      List : Util.Beans.Objects.Object;
   begin
      if Name'Length = 0 then
         List := From;
      else
         List := Util.Beans.Objects.Get_Value (From, Name);
      end if;
      Value.Clear;
      if Is_Array (List) then
         for I in 1 .. Get_Count (List) loop
            Value.Append (To_Unbounded_String (Get_Value (List, I)));
         end loop;
      end if;
   end Deserialize;

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Nullable_UString_Vectors.Vector) is
      use Util.Beans.Objects;
      List  : Util.Beans.Objects.Object;
      Item  : Util.Beans.Objects.Object;
   begin
      List :=
        (if Name'Length = 0 then From
         else Util.Beans.Objects.Get_Value (From, Name));
      Value.Clear;
      if Is_Array (List) then
         for I in 1 .. Get_Count (List) loop
            Item := Get_Value (List, I);
            Value.Append
              ((if Util.Beans.Objects.Is_Null (Item) then
                  (Is_Null => True, Value => <>)
                else (Is_Null => False, Value => To_Unbounded_String (Item))));
         end loop;
      end if;
   end Deserialize;

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Integer_Map) is
      procedure Process (Name : in String;
                         Item : in Util.Beans.Objects.Object);

      List : Util.Beans.Objects.Object;

      procedure Process (Name : in String;
                         Item : in Util.Beans.Objects.Object) is
      begin
         Value.Include (Name, Util.Beans.Objects.To_Integer (Item));
      end Process;
   begin
      List :=
        (if Name'Length = 0 then From
         else Util.Beans.Objects.Get_Value (From, Name));
      Value.Clear;
      Util.Beans.Objects.Maps.Iterate (List, Process'Access);
   end Deserialize;

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Nullable_Integer_Map) is
      procedure Process (Name : in String;
                         Item : in Util.Beans.Objects.Object);

      List : Util.Beans.Objects.Object;

      procedure Process (Name : in String;
                         Item : in Util.Beans.Objects.Object) is
      begin
         if Util.Beans.Objects.Is_Null (Item) then
            Value.Include (Name, (Is_Null => True, Value => <>));
         else
            Value.Include (Name, (Is_Null => False,
                                  Value => Util.Beans.Objects.To_Integer (Item)));
         end if;
      end Process;
   begin
      List :=
        (if Name'Length = 0 then From
         else Util.Beans.Objects.Get_Value (From, Name));
      Value.Clear;
      Util.Beans.Objects.Maps.Iterate (List, Process'Access);
   end Deserialize;

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Object_Map) is
      procedure Process (Name : in String;
                         Item : in Util.Beans.Objects.Object);

      List : Util.Beans.Objects.Object;

      procedure Process (Name : in String;
                         Item : in Util.Beans.Objects.Object) is
      begin
         Value.Include (Name, Item);
      end Process;
   begin
      List :=
        (if Name'Length = 0 then From
         else Util.Beans.Objects.Get_Value (From, Name));
      Value.Clear;
      Util.Beans.Objects.Maps.Iterate (List, Process'Access);
   end Deserialize;

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Object_Vector) is
      use Util.Beans.Objects;
      List  : Util.Beans.Objects.Object;
      Item  : Util.Beans.Objects.Object;
   begin
      if Name'Length = 0 then
         List := From;
      else
         List := Util.Beans.Objects.Get_Value (From, Name);
      end if;
      Value.Clear;
      if Is_Array (List) then
         for I in 1 .. Get_Count (List) loop
            Item := Get_Value (List, I);
            Value.Append (Item);
         end loop;
      end if;
   end Deserialize;

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Number_Vectors.Vector) is
      use Util.Beans.Objects;
      List : Util.Beans.Objects.Object;
   begin
      if Name'Length = 0 then
         List := From;
      else
         List := Util.Beans.Objects.Get_Value (From, Name);
      end if;
      Value.Clear;
      if Is_Array (List) then
         for I in 1 .. Get_Count (List) loop
            Value.Append (To_Long_Long_Float (Get_Value (List, I)));
         end loop;
      end if;
   end Deserialize;

   procedure Deserialize (From  : in OpenAPI.Value_Type;
                          Name  : in String;
                          Value : out Blob_Ref) is
      Item : OpenAPI.Value_Type;
   begin
      if Name = "" then
         Item := From;
      else
         Deserialize (From, Name, Item);
      end if;
      Value := Util.Beans.Objects.To_Blob (Item);
   end Deserialize;

end OpenAPI.Streams;
