with System;
with Interfaces.C;

package Test_Ada_Aspects is

   type R is record
      V  : Integer range 0 .. 255;
      B1 : Boolean;
      B2 : Boolean;
   end record
     with Pack;

   type Float_Int_Union (Use_Float : Boolean) is record
      case Use_Float is
         when True  => F : Float;
         when False => I : Integer;
      end case;
   end record
     with Unchecked_Union;

   type my_struct is record
      A : Interfaces.C.int;
      B : Interfaces.C.int;
   end record
     with Convention => C_Pass_By_Copy;

   procedure Initialize (Size : Integer)
     with
       Import        => True,
       Convention    => C,
       External_Name => "registerInterface_Initialize";

   type Percentage is range 0 .. 100
     with Default_Value => 10;

   type State is (Off, State_1, State_2)
     with Size => Integer'Size;

   for State use (Off     => 0,
                  State_1 => 32,
                  State_2 => 64);

   type Registers is record
      Reserved_0 : Integer;
      Reserved_1 : Integer;
   end record
     with Volatile, Size => 128;

private

   Arr_1 : array (1 .. 2) of Long_Float with Volatile;
   Arr_2 : array (1 .. 2) of Integer with Atomic_Components;

   V : Integer with
     Atomic,
     Address => System'To_Address (16#FFFF00A0#);

end Test_Ada_Aspects;
