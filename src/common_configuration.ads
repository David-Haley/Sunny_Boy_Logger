--  Configuration interface between SB_Logger and its configuration
--  program.

--  Author    : David Haley
--  Created   : 27/10/2026
--  Last_edit : 27/10/2026

package Common_Configuration is

   type Parameters is (Inverter, Start_Delay, Broker, User, Password, Topic);

   Configuration_File : constant String := "SB_Logger_Configuration.json";

   function Encrypted (Parameter : Parameters) return Boolean;

end Common_Configuration;