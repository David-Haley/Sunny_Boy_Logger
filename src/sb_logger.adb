-- Main program for logging SB 1.5 VL -40 via Modbus
-- Author    : David Haley
-- Created   : 04/04/2021
-- Last Edit :12/08/2023
-- 20230812: Handelers removed during error shutdown.
-- 20230422: Spaces removed from log file records.
-- 20230104: Frequencies supported renge changed from 0.0 .. 53 to 40 .. 60.
-- Change prompted by an exception which occored on 26/12/2022.
-- 20220909: Voltage and current ranges increased after exception raised on DC
-- voltage.
-- 20220820: Events_and_Errors moved to DJH.Events_and_Errors
-- 20220813: Modified to work as a service.

with Ada.Command_line; use Ada.Command_line;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with Linux_Signals; use Linux_Signals;
with Data_Logger; use Data_Logger;

procedure SB_Logger is

Start_Message : String := "SB_Logger 20230422 Started";

begin -- SB_Logger
   Start_Events;
   Handlers.Install;
   delay (45.0); -- Wait for DNS service availability
   if Argument_Count > 1 then
      Put_Event (Start_Message & ' ' & Argument (1) & Argument (2));
      Logger.Start (Argument (1), Argument (2));
   elsif Argument_Count > 0 then
      Put_Event (Start_Message & ' ' & Argument (1));
      Logger.Start (Argument (1));
   else
      Put_Event (Start_Message);
      Logger.Start ("SMA1930015238");
   end if; -- Argument_Count > 1
   loop
      delay 1.0;
      exit when Handlers.Signal_Stop or Ctrl_C_Stop;
   end loop;
   Handlers.Remove;
   Logger.Stop;
   Put_Event ("Normal exit");
   Stop_Events;
exception
   when E : others =>
      Put_Error ("Unhandled error", E);
      Put_Event ("Error exit");
      Handlers.Remove;
      Stop_Events;
      Logger.Stop;
end SB_Logger;
