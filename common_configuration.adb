--  Configuration interface between SB_Logger and its configuration
--  program.

--  Author    : David Haley
--  Created   : 28/10/2026
--  Last_edit : 28/10/2026

package body Common_Configuration is

   function Encrypted (Parameter : Parameters) return Boolean is
     (Parameter = Password);

end Common_Configuration;