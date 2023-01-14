-- This package provides data logging for the SMA SB1.5-VL-40
-- Author    : David Haley
-- Created   : 04/04/2021
-- Last Edit : 13/08/2022
-- 20220813: Provide for Inverter Name and Port being set by caller.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

package Data_Logger is
   
   task Logger is
      -- Logger task declaration
      entry Start  (Inverter_Name : in String; Port : in String := "");
      entry Stop;
   end Logger;

   function Read_File_Commit_Time return Time;
   -- Returns time of next file commit, that is, Flush (xx)

   function On_The_Hour (T : in Time) return Time;
   -- Effectively rounds T down such that minutes and seconds are zero

end Data_Logger;
