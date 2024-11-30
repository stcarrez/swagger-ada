--  REST API types validation
--  Types to validate
--
--  The version of the OpenAPI document: 1.0.0
--
--
--  NOTE: This package is auto generated by OpenAPI-Generator 7.11.0-2024-11-24.
--  https://openapi-generator.tech
--  Do not edit the class manually.

with Types.Models;
with OpenAPI.Clients;
package Types.Clients is
   pragma Style_Checks ("-bmrIu");

   type Client_Type is new OpenAPI.Clients.Client_Type with null record;

   --
   procedure Add_Rack
     (Client    : in out Client_Type;
      Rack_Info : in     Types.Models.RackInfo_Type;
      Result    :    out Types.Models.RackInfo_Type);

   --
   procedure Get_Rack
     (Client : in out Client_Type;
      Name   : in     OpenAPI.Long;
      Result :    out Types.Models.RackInfo_Type);

   --
   procedure Update_Rack
     (Client    : in out Client_Type;
      Name      : in     OpenAPI.Long;
      Rack_Info : in     Types.Models.RackInfo_Type;
      Result    :    out Types.Models.RackInfo_Type);

end Types.Clients;