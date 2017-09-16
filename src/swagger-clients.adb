-----------------------------------------------------------------------
--  swagger-clients -- Rest client support
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
package body Swagger.Clients is

   --  Set the path to use for the URI.
   procedure Set_Path (URI  : in out URI_Type;
                       Path : in String) is
   begin
      null;
   end Set_Path;

   --  Set the path parameter.
   procedure Set_Path_Param (URI   : in out URI_Type;
                             Name  : in String;
                             Value : in String) is
   begin
      null;
   end Set_Path_Param;

   --  Set the path parameter.
   procedure Set_Path_Param (URI   : in out URI_Type;
                             Name  : in String;
                             Value : in UString) is
   begin
      null;
   end Set_Path_Param;

   --  Add a query parameter.
   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in String) is
   begin
      null;
   end Add_Param;

   --  Add a query parameter.
   procedure Add_Param (URI   : in out URI_Type;
                        Name  : in String;
                        Value : in UString) is
   begin
      null;
   end Add_Param;

   procedure Call (Client    : in out Client_Type;
                   Operation : in Operation_Type;
                   URI       : in URI_Type'Class;
                   Request   : in Request_Type) is
   begin
      null;
   end Call;

   procedure Call (Client    : in out Client_Type;
                   Operation : in Operation_Type;
                   URI       : in URI_Type'Class) is
   begin
      null;
   end Call;

   --  Set the Accept header according to what the operation supports and what is
   --  selected by the client.
   procedure Set_Accept (Client : in out Client_Type;
                         List   : in Content_Type_Array) is
   begin
      null;
   end Set_Accept;

end Swagger.Clients;
