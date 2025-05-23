-- This package provides data logging for the SMA SB1.5-VL-40
-- Author    : David Haley
-- Created   : 04/04/2021
-- Last Edit : 27/04/2025

-- 20250427: Added checks for SMA NaN values.
-- 20250301: Provision made to restart connection after a Modbus error and fine
-- grained exception handling provided on SMA type conversions.
-- 20230923: When an exception occurs the logging file and Modbus connection are 
-- explicitly closed.
-- 20230612: exception handlers reraise exception.
-- 20230422: Spaces removed from log file records and space removed from header.
-- 20230104: Frequencies supported renge changed from 0.0 .. 53 to 40 .. 60.
-- Change prompted by an exception which occored on 26/12/2022.
-- 20220909: Voltage and current ranges increased after exception raised on DC
-- voltage.
-- 20220820: Localised exception handling added.
-- 20220813: Provide for Inverter Name and Port being set by caller.
-- 20210406 : Supress logging of zero DC_Current or when DC_Current is SMA's
-- NAN for U_32.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Calendar.Time_Zones; use Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;
with GNAT.Sockets; use GNAT.Sockets;
with DJH.Modbus; use DJH.Modbus;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;

package body Data_Logger is
   Logging_File : File_Type;
   Log_Interval : constant Day_Duration := 60.0;
   -- create a log entry at 1 minute inerevals
   File_Commit_Interval : constant Duration := 3600.0;
   -- Commit logging files once per hour

   function On_The_Hour (T : in Time) return Time is
      -- Effectively rounds T down such that minutes and seconds are zero

      Year : Year_Number;
      Month : Month_Number;
      Day : Day_Number;
      This_Hour : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
      Sub_Second : Second_Duration;
      Leap_Second : Boolean;

   begin -- On_The_Hour
      Split (T, Year, Month, Day,
             This_Hour, Minute, Second, Sub_Second,
             Leap_Second, UTC_Time_Offset (T));
      return Time_Of (Year, Month, Day,
                      This_Hour, 0, 0, 0.0,
                      Leap_Second, UTC_Time_Offset (T));
   end On_The_Hour;

   protected type File_Commit_Times is

      procedure Set_Next_File_Commit;
      -- Sets time for next file commit

      function Get_Next_File_Commit return Time;
      -- Gets time of next file commit

   private
      File_Commit_Time : Time := On_The_Hour (Clock) + File_Commit_Interval;
      -- Once per hour on the hour, first commit could beless than one hour
      -- after start
   end File_Commit_Times;

   protected body File_Commit_Times is

      procedure Set_Next_File_Commit is
         -- Sets time for next file commit

      begin -- Set_Next_File_Commit
         File_Commit_Time := File_Commit_Time +File_Commit_Interval;
      end Set_Next_File_Commit;

      function Get_Next_File_Commit return Time is
         -- Gets time of next file commit

      begin -- Get_Next_File_Commit
         return File_Commit_Time;
      end Get_Next_File_Commit;

   end File_Commit_Times;

   Logging_File_Commit_Time : File_Commit_Times;

   function Read_File_Commit_Time return Time is
      -- Returns time of next file commit, that is, Flush (xx)

   begin -- Read_File_Commit_Time
      return Logging_File_Commit_Time.Get_Next_File_Commit;
   end Read_File_Commit_Time;
   
   function Logging_Path (This_Time : Time) return string is
      -- returns "YYYY" representing the current year

   begin -- Logging_Path
      return Reverse_Date_String (This_Time) (1..4);
   end Logging_Path;

   function Logging_File_Name (This_Time : Time) return string is
      -- returns "YYYYMMDD.csv" where YYYYMMDD represents the current date

   begin -- Logging_File_Name
      return '/' & Reverse_Date_String (This_Time) & ".csv";
   end Logging_File_Name;

   procedure Open_Log_File (Logging_File : in out File_Type;
                            This_Time : in Time) is

   begin -- Open_Log_File
      if not (Exists (Logging_Path (This_Time))) then
         Create_Directory (Logging_Path (This_Time));
         -- exceptions may be raised later if the name exists and is a file not
         -- a directory
      end if; -- not (Exists (Logging_Path (This_Time)))
      if Exists (Logging_Path (This_Time) & Logging_File_Name (This_Time)) then
         Open (Logging_File, Append_File, Logging_Path  (This_Time) &
                 Logging_File_Name (This_Time));
      else
         Create (Logging_File, Out_File, Logging_Path (This_Time) &
                   Logging_File_Name (This_Time));
         Put_Line (Logging_File, "Time,DC Current,DC Voltage," &
                     "DC Power,AC Current,AC Voltage,AC Power," &
                     "Frequency,Daily Yield,Total Yield");
         -- write header in logging file
      end if; -- Exists (Logging_Path (This_Time) & ...
   exception
      when E : others =>
         Put_Error ("Open_Log_File", E);
         raise;
   end Open_Log_File;

   function Is_Next_Day (Old_Time, New_Time : Time) return Boolean is

      Year : Year_Number;
      Month  : Month_Number;
      Day, Next_Day : Day_Number;
      Seconds : Day_Duration;

   begin -- Is_Next_Day
      Split (Old_Time, Year, Month, Day, Seconds);
      Split (New_Time, Year, Month, Next_Day, Seconds);
      return Next_Day /= Day;
   end Is_Next_Day;

   function Calculate_Next_Log_Entry return Time is

      Year : Year_Number;
      Month  : Month_Number;
      Day : Day_Number;
      Seconds : Day_Duration;
      This_Time : Time;

   begin -- Calculate_Next_Log_Entry
      This_Time := Clock;
      if Is_Next_Day (This_Time, This_Time + Log_Interval) then
         Split (This_Time + Log_Interval, Year, Month, Day, Seconds);
         return Ada.Calendar.Time_Of (Year, Month, Day, 0.0);
      else
         Split (This_Time, Year, Month, Day, Seconds);
         Seconds := Duration (Integer (Seconds / Log_Interval)) * Log_Interval
           + Log_Interval;
         return Ada.Calendar.Time_Of (Year, Month, Day, Seconds);
      end if; -- Next_Day (This_Time, This_Time + Log_Interval)
   end Calculate_Next_Log_Entry;

   Procedure Start_Logging (Logging_File : in out File_Type;
                            Next_Log_Entry : out Time) is

   begin -- Start_Logging
      Next_Log_Entry := Calculate_Next_Log_Entry;
      Open_Log_File (Logging_File, Next_Log_Entry);
   end Start_Logging;

   procedure Put_Log_Entry (Logging_File : in out File_Type) is

      type Currents is delta 0.001 range 0.0 .. 12000.0;
      -- 1000 times required range to allow for division by 1000.0
      -- Supports range to 12 A to allow for SB2.5 AC current
      type Voltages is delta 0.01 range 0.0 .. 60000.0;
      -- 100 times required range to allow for division by 100
      -- Supported range up to 600 V to cover maximum permitted DC panel voltage
      type Frequencies  is delta 0.01 range 40.0 .. 6000.0;
      -- Supported range 40.00 to 60.00 Hz

      package Current_IO is new Ada.Text_IO.Fixed_IO (Currents);
      package Voltage_IO is new Ada.Text_IO.Fixed_IO (Voltages);
      package Frequency_IO is new Ada.Text_IO.Fixed_IO (Frequencies);

      Unit_Id : constant Unit_Ids := 3;
      NaN_S32 : constant Unsigned_32 := 16#80000000#;
      NaN_U32 : constant Unsigned_32 := 16#FFFFFFFF#;
      NaN_U64 : constant Unsigned_64 := 16#FFFFFFFFFFFFFFFF#;
      Yield : Register_Arrays (30513 .. 30520);
      Power : Register_Arrays (30769 .. 30776);
      L1_Current : Register_Arrays (30977 .. 30978);
      L1_Voltage : Register_Arrays (30783 .. 30784);
      Grid_Frequency : Register_Arrays (30803 .. 30804);
      DC_Current, AC_Current : Currents;
      DC_Voltage, AC_Voltage : Voltages;
      Frequency : Frequencies;
      Out_String : String (1 .. 15);
      Log_Entry : Unbounded_String := To_Unbounded_String (Time_String & ",");
      Valid_Entry : Boolean;
      Error_Prefix : constant String := "Put_Log_Entry - ";

   begin -- Put_Log_Entry
      Read_Registers (Unit_Id, Yield);
      Read_Registers (Unit_Id, Power);
      Read_Registers (Unit_Id, L1_Current);
      Read_Registers (Unit_Id, L1_Voltage);
      Read_Registers (Unit_Id, Grid_Frequency);
      Valid_Entry := To_U32 (Power (30775 .. 30776)) > 0 and -- AC power
        To_U32 (Power (30775 .. 30776)) /= NaN_S32 and
        To_U32 (Power (30769 .. 30770)) /= NaN_S32 and -- DC current
        To_U32 (Power (30771 .. 30772)) /= NaN_S32 and -- DC voltage
        To_U32 (Power (30773 .. 30774)) /= NaN_S32 and -- DC power
        To_U32 (L1_Current) /= NaN_S32 and To_U32 (L1_Voltage) /= NaN_U32 and
        To_U32 (Grid_Frequency) /= NaN_U32 and
        To_U64 (Yield (30513 ..30516)) /= NaN_U64 and -- Total yield
        To_U64 (Yield (30517 ..30520)) /= NaN_U64; --
      if Valid_entry then
         begin -- DC_Current exception block
            DC_Current := Currents (To_U32 (Power (30769 .. 30770))) / 1000.0;
         exception
            when E : others =>
               Put_Error (Error_Prefix & "DC_Current", E);
               Valid_Entry := False;
         end; -- DC_Current exception block
         begin -- DC_Voltage exceptiob block
            DC_Voltage := Voltages (To_U32 (Power (30771 .. 30772))) / 100.0;
         exception
            when E : others =>
               Put_Error (Error_Prefix & "DC_Voltage", E);
               Valid_Entry := False;
         end; -- DC_Voltage exceptiob block
         begin -- AC_Current exception block
            AC_Current := Currents (To_U32 (L1_Current)) / 1000.0;
         exception
            when E : others =>
               Put_Error (Error_Prefix & "AC_Current", E);
               Valid_Entry := False;
         end; -- AC_Current exception block
         begin -- AC_Voltage exception bloc
           AC_Voltage := Voltages (To_U32 (L1_Voltage)) / 100.0;
         exception
            when E : others =>
               Put_Error (Error_Prefix & "AC_Voltage", E);
               Valid_Entry := False;
         end; -- AC_Voltage exception block
         begin -- Frequency exception block
            Frequency := Frequencies (To_U32 (Grid_Frequency)) / 100.0;
         exception
            when E : others =>
               Put_Error (Error_Prefix & "Frequency", E);
               Valid_Entry := False;
         end; -- Frequency exception block
      end if; -- Valid_Entry
      if Valid_Entry then
         Current_IO.Put (Out_String, DC_Current, 3, 0);
         Log_Entry := Log_Entry & Trim (Out_String, Both) & ',';
         Voltage_IO.Put (Out_String, DC_Voltage, 2, 0);
         Log_Entry := Log_Entry & Trim (Out_String, Both) & ',' &
           Trim (To_U32 (Power (30773 .. 30774))'Img, Both) & ','; -- DC Power
         Current_IO.Put (Out_String, AC_Current, 3, 0);
         Log_Entry := Log_Entry & Trim (Out_String, Both) & ',';
         Voltage_IO.Put (Out_String, AC_Voltage, 2, 0);
         Log_Entry := Log_Entry & Trim (Out_String, Both) & ',' &
           Trim (To_U32 (Power (30775 .. 30776))'Img, Both) & ','; -- AC Power
         Frequency_IO.Put (Out_String, Frequency, 2, 0);
         Log_Entry := Log_Entry & Trim (Out_String, Both) & ',';
         Log_Entry := Log_Entry &
           Trim (To_U64 (Yield (30517 .. 30520))'Img, Both) & ',' & -- Day Yield
           Trim (To_U64 (Yield (30513 .. 30516))'Img, Both); -- Total Yield
         Put_Line (Logging_File, Log_Entry);
      end if; -- Valid_Entry
   exception
      when E : others =>
         Put_Error (Error_Prefix & "unhandled exception", E);
         raise;
   end Put_Log_Entry;

   task body Logger is
   
      Inverter_Name_Store, Port_Store : Unbounded_String;
      Run_Logger : Boolean := True;
      Next_Time, Previous_Time : Time;

   begin -- Logger
      accept Start (Inverter_Name : in String; Port : in String := "") do
         Inverter_Name_Store := To_Unbounded_String (Inverter_Name);
         Port_Store := To_Unbounded_String (Port);
         Start_Logging (Logging_File, Next_Time);
         Previous_Time := Next_Time - Log_Interval;
         if Length (Port_Store) = 0 then
            Connect (To_String (Inverter_Name_Store));
         else
            Connect (To_String (Inverter_Name_store),
                     Port_Type'Value (To_String (Port_Store)));
         end if; -- Length (Port_Store) = 0
      end Start;
      While Run_Logger loop
         select
            accept Stop do
               Run_logger := False;
               Close (Logging_File);
               Close_Connection;
            end Stop;
         or
            delay until Next_Time;
            if Clock - Next_Time > 2 * Log_Interval then
               -- a big step forward in time has occurred
               Next_Time := Calculate_Next_Log_Entry;
            end if; -- Next_Time - Clock > Log_Interval
            if Is_Next_Day (Previous_Time, Next_Time) then
               -- Day has changed hence a new logging file is required
               Close (Logging_File);
               Previous_Time := Next_Time;
               -- N.B. Start_Logging updates Next_Time!
               Start_Logging (Logging_File, Next_Time);
            else
               Previous_Time := Next_Time;
               Next_Time := Next_Time + Log_Interval;
            end if; -- Is_Next_Day (Previous_Time, Next_Time)
            begin -- Read_innverter_exception block
               Put_Log_Entry (Logging_File);
            exception
               when Modbus_Error =>
                  Close_Connection;
                  Delay 30.0;
                  if Length (Port_Store) = 0 then
                     Connect (To_String (Inverter_Name_Store));
                  else
                     Connect (To_String (Inverter_Name_store),
                              Port_Type'Value (To_String (Port_Store)));
                  end if; -- Length (Port_Store) = 0
                  Put_Event ("Restarting Modbus connection");
            end; -- Read_innverter_exception block
            if Clock >= Logging_File_Commit_Time.Get_Next_File_Commit then
               Flush (Logging_File);
               Logging_File_Commit_Time.Set_Next_File_Commit;
            end if; -- Clock >= File_Commit_Time
         end select;
      end loop;
   exception
      when E : others =>
         Put_Error ("Logger", E);
         Close (Logging_File);
         Close_Connection;
         raise;
   end Logger;

end Data_Logger;
