-- Main program for logging SB 1.5 VL -40 via Modbus
-- Author    : David Haley
-- Created   : 04/04/2021
-- Last Edit : 27/10/2026

--  20261027: Publishes logger parameters as a MQTT stream. Configuration is now
--  read from a configuration file, default Modbus port is assumed.
-- 20261026: Logger task supervised, if it terminates or stops updating its
-- heartbeat the program exits with failure status, allowing systemd to restart
-- the service.
-- 20250427: Added checks for SMA NaN values.
-- 20250418: Rebuilt with updated DJH.Events_and_Errors.
-- 20250301: Provision made to restart connection after a Modbus error and fine
-- grained exception handling provided on SMA type conversions.
-- 20230923: Some tiding up, inclusion of Default Inverter, change to Logger to
-- improve the probability of termination when errors occur.
-- 20230909: Changed order in termination in exception handler. Forced
-- termination of logger task provided.
-- 20230812: Handelers removed during error shutdown.
-- 20230422: Spaces removed from log file records.
-- 20230104: Frequencies supported renge changed from 0.0 .. 53 to 40 .. 60.
-- Change prompted by an exception which occored on 26/12/2022.
-- 20220909: Voltage and current ranges increased after exception raised on DC
-- voltage.
-- 20220820: Events_and_Errors moved to DJH.Events_and_Errors
-- 20220813: Modified to work as a service.

with GNAT.OS_Lib;
with DJH.Events_and_Errors; use DJH.Events_and_Errors;
with Linux_Signals; use Linux_Signals;
with Common_Configuration; use Common_Configuration;
with DJH.JSON_Configuration;
with Data_Logger; use Data_Logger;

procedure SB_Logger is

   package Configuration is new
      DJH.JSON_Configuration (Parameters, Configuration_File, Encrypted);
   use Configuration;

   Start_Message : String := "SB_Logger 20261026 Started, Inverter: ";
   Default_Inverter : String := "SMA1930015238";
   Heartbeat_Timeout : constant Duration := 300.0;
   -- Five logging intervals, all Modbus operations are time limited

   function Logger_Failed return Boolean is
   (Logger'Terminated or else Heartbeat_Age > Heartbeat_Timeout);

begin -- SB_Logger
   Put_Line (Start_Message);
   Put_Event (Start_Message);
   if Configuration_File_Exists then
      Read_Configuration;
      Put_Line ("Read " & Configuration_File);
      Put_Event ("Read " & Configuration_File);
   else
      raise JSON_Configuration_Error with "Missing configuration file " &
        Configuration_File;
   end if; -- Configuration_File_Exists
   Handlers.Install;
   delay Duration'Value (Get_Value (Start_Delay));
   --  Origionally provided to wait for DNS service availability, this can be
   --  covered by correctly configuring the systemd service to wait for
   --  network availability. A delay may be desirable to provide for the
   --  inverter to start and connect to the network.
   Logger.Start (Get_Value (Inverter),
                 Get_Value (Broker),
                 Get_Value (User),
                 Get_Value (Password),
                 Get_Value (Topic));
   Put_Event ("Logger task started");
   loop
      delay 1.0;
      exit when Handlers.Signal_Stop or Ctrl_C_Stop or Logger_Failed;
   end loop;
   Handlers.Remove;
   if not (Handlers.Signal_Stop or Ctrl_C_Stop) then
      Put_Event ("Logger task failed or hung, exiting to allow restart");
      abort Logger;
      Put_Event ("Error exit");
      Stop_Events;
      GNAT.OS_Lib.OS_Exit (1);
      -- Does not wait for the Logger task, which may be blocked
   else
      Logger.Stop;
      Put_Event ("Normal exit");
      Stop_Events;
   end if; -- not (Handlers.Signal_Stop or Ctrl_C_Stop)
exception
   when E : others =>
      Put_Error ("Unhandled error", E);
      Put_Event ("Aborting Logger task");
      abort Logger;
      Put_Event ("Removing signal handlers");
      Handlers.Remove;
      Put_Event ("Error exit");
      Stop_Events;
      GNAT.OS_Lib.OS_Exit (1);
end SB_Logger;
