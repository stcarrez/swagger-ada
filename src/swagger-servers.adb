-----------------------------------------------------------------------
--  swagger-server -- Rest server support
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

package body Swagger.Servers is

   --  ------------------------------
   --  Get a request parameter defined in the URI path.
   --  ------------------------------
   procedure Get_Path_Parameter (Req   : in Request'Class;
                                 Pos   : in Positive;
                                 Value : out UString) is
   begin
      Value := To_UString (Req.Get_Path_Parameter (Pos));
   end Get_Path_Parameter;

   --  ------------------------------
   --  Get a request parameter defined in the URI path.
   --  ------------------------------
   procedure Get_Path_Parameter (Req   : in Request'Class;
                                 Pos   : in Positive;
                                 Value : out Long) is
      V : constant String := Req.Get_Path_Parameter (Pos);
   begin
      Value := Long'Value (V);
   end Get_Path_Parameter;

end Swagger.Servers;
