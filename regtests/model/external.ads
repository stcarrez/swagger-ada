with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
package External is

   type Stat_Type is record
      Count : Natural;
      Name  : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Stat_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Stat_Type);

   subtype Stat_Vector is Stat_Vectors.Vector;

end External;
