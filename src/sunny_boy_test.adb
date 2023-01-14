-- Test the ability to read SB 1.5 VL -40 Modbus registers
-- Author    : David Haley
-- Created   : 25/02/2021
-- Last Edit : 04/04/2021

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets; use GNAT.Sockets;
with DJH.Modbus; use DJH.Modbus;

procedure Sunny_Boy_Test is

   type Currents is delta 0.001 range 0.0 .. 10000.0;
   -- 1000 times required range to allow for division by 1000.0
   type Voltages is delta 0.01 range 0.0 .. 28000.0;
   -- 100 times required range to allow for division by 100.0
   type Frequencies  is delta 0.01 range 0.0 .. 5300.0;

   package Current_IO is new Ada.Text_IO.Fixed_IO (Currents);
   package Voltage_IO is new Ada.Text_IO.Fixed_IO (Voltages);
   package Frequency_IO is new Ada.Text_IO.Fixed_IO (Frequencies);

   SB1_5_Address : constant Inet_Addr_Type := (Family => Family_Inet,
                                               Sin_V4 => (10, 0, 0, 136));
   Unit_Id : constant Unit_Ids := 3;
   Yeild : Register_Arrays (30513 .. 30520);
   Power : Register_Arrays (30769 .. 30776);
   L1_Current : Register_Arrays (30977 .. 30978);
   L1_Voltage : Register_Arrays (30783 .. 30784);
   Grid_Frequency : Register_Arrays(30803 .. 30804);
   DC_Current, AC_Current : Currents;
   DC_Voltage, AC_Voltage : Voltages;
   Frequency : Frequencies;


begin -- Sunny_Boy_Test
   Put_Line ("Connecting)");
   Connect (SB1_5_Address);
   loop -- one minute read
      Put_Line ("Reading");
      Read_Registers (Unit_Id, Yeild);
      Read_Registers (Unit_Id, Power);
      Read_Registers (Unit_Id, L1_Current);
      Read_Registers (Unit_Id, L1_Voltage);
      Read_Registers (Unit_Id, Grid_Frequency);
      Put_Line ("Total Yield:" & To_U64 (Yeild (30513 .. 30516))'Img);
      Put_Line ("Daily Yield:" & To_U64 (Yeild (30517 .. 30520))'Img);
      DC_Current := Currents (To_U32 (Power (30769 .. 30770))) / 1000.0;
      Put ("DC Current:");
      Current_IO.Put (DC_Current, 3, 3, 0);
      New_Line;
      DC_Voltage := Voltages (To_U32 (Power (30771 .. 30772))) / 100.0;
      Put ("DC Voltage:");
      Voltage_IO.Put (DC_Voltage, 4, 2, 0);
      New_Line;
      Put_Line ("DC Power:" & To_U32 (Power (30773 .. 30774))'Img);
      AC_Current := Currents (To_U32 (L1_Current)) / 1000.0;
      Put ("AC Current:");
      Current_IO.Put (AC_Current, 3, 3, 0);
      New_Line;
      AC_Voltage := Voltages (To_U32 (L1_Voltage)) / 100.0;
      Put ("AC Voltage:");
      Voltage_IO.Put (AC_Voltage, 4, 2, 0);
      New_Line;
      Put_Line ("AC Power:" & To_U32 (Power (30775 .. 30776))'Img);
      Frequency := Frequencies (To_U32 (Grid_Frequency)) / 100.0;
      Put ("Frequency:");
      Frequency_IO.Put (Frequency, 3, 2, 0);
      New_Line;
      delay (60.0);
   end loop; -- one minute read
end Sunny_Boy_Test;
