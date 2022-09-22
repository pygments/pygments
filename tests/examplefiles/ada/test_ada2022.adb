with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Test_Ada2022 is

   package Integer_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Integer);

   use Integer_Vectors;

   procedure Increment_All (V : in out Vector) is
   begin
      for E of V loop
         E := @ + 1;
      end loop;
   end Increment_All;

   V : Vector := [0, 0, 0];

begin

   Increment_All (V);
   Put_Line (V'Image);

end Test_Ada2022;
