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
package body Swagger.Streams is

   --  ------------------------------
   --  Serialize a list of strings in the stream.
   --  ------------------------------
   procedure Serialize (Stream : in out Output_Stream'Class;
                        Name   : in String;
                        Value  : in Swagger.UString_Vectors.Vector) is
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
                        Value  : in Swagger.Long) is
   begin
      Stream.Write_Long_Entity (Name, Value);
   end Serialize;

   --  ------------------------------
   --  Extract a boolean value stored under the given name.
   --  ------------------------------
   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out Boolean) is
      Item : Swagger.Value_Type;
   begin
      if Name = "" then
         Item := From;
      else
         Deserialize (From, Name, Item);
      end if;
      Value := Util.Beans.Objects.To_Boolean (Item);
   end Deserialize;

   --  ------------------------------
   --  Extract an integer value stored under the given name.
   --  ------------------------------
   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out Integer) is
      Item : Swagger.Value_Type;
   begin
      if Name = "" then
         Item := From;
      else
         Deserialize (From, Name, Item);
      end if;
      Value := Util.Beans.Objects.To_Integer (Item);
   end Deserialize;

   --  ------------------------------
   --  Extract an integer value stored under the given name.
   --  ------------------------------
   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out Long) is
      Item : Swagger.Value_Type;
   begin
      if Name = "" then
         Item := From;
      else
         Deserialize (From, Name, Item);
      end if;
      Value := Util.Beans.Objects.To_Long_Long_Integer (Item);
   end Deserialize;

   --  ------------------------------
   --  Extract a string value stored under the given name.
   --  ------------------------------
   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out UString) is
      Item : Swagger.Value_Type;
   begin
      if Name = "" then
         Item := From;
      else
         Deserialize (From, Name, Item);
      end if;
      Value := Util.Beans.Objects.To_Unbounded_String (Item);
   end Deserialize;

   --  ------------------------------
   --  Extract a value stored under the given name.
   --  ------------------------------
   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out Value_Type) is
   begin
      if Name'Length = 0 then
         Value := From;
      else
         Value := Util.Beans.Objects.Get_Value (From, Name);
      end if;
   end Deserialize;

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out Value_Array_Type) is
   begin
      null;
   end Deserialize;

   --  Extract an integer value stored under the given name.
   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out Ada.Calendar.Time) is
   begin
      null;
   end Deserialize;

   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out UString_Vectors.Vector) is
   begin
      null;
   end Deserialize;

   procedure Deserialize (From  : in Swagger.Value_Type;
                          Name  : in String;
                          Value : out Integer_Map) is
   begin
      null;
   end Deserialize;

end Swagger.Streams;
