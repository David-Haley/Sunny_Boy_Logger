--  This package provides data logging for the SMA SB1.5-VL-40
--  Author    : David Haley
--  Created   : 04/04/2021
--  Last Edit : 30/10/2026

--  20261030 : Port parameter removed and provision for MQTT brker added.
--  20261026: Heartbeat_Age added to allow detection of a failed Logger task.
--  20220813: Provide for Inverter Name and Port being set by caller.

with Ada.Calendar; use Ada.Calendar;

package Data_Logger is
   
   task Logger is
      --  Logger task declaration
      entry Start  (Inverter_Name : in String;
                    Broker : in String;
                    User : in String;
                    Password: in String;
                    Topic : in String);
      entry Stop;
   end Logger;

   function Heartbeat_Age return Duration;
   -- Returns the time since the Logger task last completed a logging cycle
   -- or was started. Monotonic, unaffected by changes to the system clock.

   function Read_File_Commit_Time return Time;
   -- Returns time of next file commit, that is, Flush (xx)

   function On_The_Hour (T : in Time) return Time;
   -- Effectively rounds T down such that minutes and seconds are zero

end Data_Logger;
