-----------------------------------------------------------------------
--  openapi-streams-forms -- x-www-form-urlencoded streams
--  Copyright (C) 2018, 2022, 2023 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

package body OpenAPI.Streams.Forms is

   procedure Initialize (Stream : in out Output_Stream;
                         Buffer : in Util.Streams.Texts.Print_Stream_Access) is
   begin
      Stream.Stream := Buffer;
   end Initialize;

   --  ------------------------------
   --  Flush the buffer (if any) to the sink.
   --  ------------------------------
   overriding
   procedure Flush (Stream : in out Output_Stream) is
   begin
      Stream.Stream.Flush;
   end Flush;

   --  ------------------------------
   --  Close the sink.
   --  ------------------------------
   overriding
   procedure Close (Stream : in out Output_Stream) is
   begin
      Stream.Stream.Close;
   end Close;

   --  ------------------------------
   --  Write the buffer array to the output stream.
   --  ------------------------------
   overriding
   procedure Write (Stream : in out Output_Stream;
                    Buffer : in Ada.Streams.Stream_Element_Array) is
   begin
      Stream.Stream.Write (Buffer);
   end Write;

   --  Write the attribute name/value pair.
   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in String) is
   begin
      if Stream.Has_Param then
         Stream.Stream.Write ('&');
      end if;
      Stream.Has_Param := True;
      Stream.Stream.Write (Name);
      Stream.Stream.Write ('=');
      Stream.Stream.Write (Value);
   end Write_Attribute;

   overriding
   procedure Write_Wide_Attribute (Stream : in out Output_Stream;
                                   Name   : in String;
                                   Value  : in Wide_Wide_String) is
   begin
      if Stream.Has_Param then
         Stream.Stream.Write ('&');
      end if;
      Stream.Has_Param := True;
      Stream.Stream.Write (Name);
      Stream.Stream.Write ('=');
      for C of Value loop
         Stream.Stream.Write_Wide (C);
      end loop;
   end Write_Wide_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Integer) is
   begin
      if Stream.Has_Param then
         Stream.Stream.Write ('&');
      end if;
      Stream.Has_Param := True;
      Stream.Stream.Write (Name);
      Stream.Stream.Write ('=');
      Stream.Stream.Write (Util.Strings.Image (Value));
   end Write_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Boolean) is
   begin
      if Stream.Has_Param then
         Stream.Stream.Write ('&');
      end if;
      Stream.Has_Param := True;
      Stream.Stream.Write (Name);
      Stream.Stream.Write ('=');
      Stream.Stream.Write (if Value then "true" else "false");
   end Write_Attribute;

   overriding
   procedure Write_Attribute (Stream : in out Output_Stream;
                              Name   : in String;
                              Value  : in Util.Beans.Objects.Object) is
   begin
      null;
   end Write_Attribute;

   --  Write the attribute with a null value.
   overriding
   procedure Write_Null_Attribute (Stream : in out Output_Stream;
                                   Name   : in String) is
   begin
      null;
   end Write_Null_Attribute;

   --  Write the entity value.
   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in String) is
   begin
      Stream.Write_Attribute (Name, Value);
   end Write_Entity;

   overriding
   procedure Write_Wide_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in Wide_Wide_String) is
   begin
      Stream.Write_Wide_Attribute (Name, Value);
   end Write_Wide_Entity;

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Boolean) is
   begin
      Stream.Write_Attribute (Name, Value);
   end Write_Entity;

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Integer) is
   begin
      Stream.Write_Attribute (Name, Value);
   end Write_Entity;

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Ada.Calendar.Time) is
   begin
      null;
   end Write_Entity;

   overriding
   procedure Write_Long_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in Long_Long_Integer) is
   begin
      null;
   end Write_Long_Entity;

   overriding
   procedure Write_Long_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in Long_Long_Float) is
   begin
      null;
   end Write_Long_Entity;

   overriding
   procedure Write_Enum_Entity (Stream : in out Output_Stream;
                                Name   : in String;
                                Value  : in String) is
   begin
      null;
   end Write_Enum_Entity;

   overriding
   procedure Write_Entity (Stream : in out Output_Stream;
                           Name   : in String;
                           Value  : in Util.Beans.Objects.Object) is
   begin
      null;
   end Write_Entity;

   --  Write an entity with a null value.
   overriding
   procedure Write_Null_Entity (Stream : in out Output_Stream;
                                Name   : in String) is
   begin
      null;
   end Write_Null_Entity;

end OpenAPI.Streams.Forms;
