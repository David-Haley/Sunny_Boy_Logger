--  Configuration program for SB_Logger.

--  Author    : David Haley
--  Created   : 27/10/2026
--  Last_edit : 31/10/2026

--  20261031 : Existing configuration with empty values can be edited, response
--  to creating a new file requires enter.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Common_Configuration; use Common_Configuration;
with DJH.JSON_Configuration;

procedure Configure_SB_Logger is

   package Configuration is new
      DJH.JSON_Configuration (Parameters, Configuration_File, Encrypted);
   use Configuration;

   Editing : Boolean := False;
   Response : Character := 'Y';
   Value : Unbounded_String;

   function Value_Or_Empty (Parameter : Parameters) return String is
      --  Get_Value raises JSON_Configuration_Error if the value is empty

   begin -- Value_Or_Empty
      return Get_Value (Parameter);
   exception
      when JSON_Configuration_Error =>
         return "";
   end Value_Or_Empty;

begin -- Configure_SB_Logger
   Put_Line ("Configure_SB_Logger version 20261031");
   if Configuration_File_Exists then
      Put_Line ("Configuration file " & Configuration_File & " found");
      Put_Line ("For each prompt either enter a new value or enter only to");
      Put_Line ("retain the existing value.");
      Read_Configuration;
      Editing := True;
   else
      Put_Line ("Configuration file " & Configuration_File & " not found");
      Put ("Create a new configuration file [y | n] : ");
      Get (Response);
      Skip_Line;
      if Response = 'y' or Response = 'Y' then
         Put_Line ("Enter a value for each prompt, User and Password may be");
         Put_Line ("left empty if the broker does not require them.");
      end if; -- Response = 'y' or Response = 'Y'
   end if; -- Configuration_File_Exists
   if Response = 'y' or Response = 'Y' then
      for P in Parameters loop
         if Editing and not Encrypted (P) then
            Put_Line (P'Img & " """ & Value_Or_Empty (P) & """");
         end if; -- Editing and not Encrypted (P)
         Put (P'Img & ": ");
         Get_Line (Value);
         if Editing and Length (Value) = 0 then
            null;
         else
            Set_Value (P, To_String (Value));
         end if; -- Editing and Length (Value) = 0
      end loop; -- P in Parameters
      Write_Configuration;
   end if; -- Response = 'y' or Response = 'Y'
end Configure_SB_Logger;