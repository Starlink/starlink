      BLOCK DATA MAG1_BLK
*+
*  Name:
*     MAG1_BLK
 
*  Purpose:
*     MAG Block Data Initialisation.
 
*  Language:
*     Starlink Fortran
 
*  Invocation:
*     No invocation for Block Data
 
*  Description:
*     Initialise the MAGIO Common Block so that implicit activation
*     of MAG can be done.
 
*  Authors:
*     Jack Giddings (UCL::JRG)
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}
 
*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*    14-Jul-1983:  Re-structured. (UCL::SLW)
*    14-Nov-1991:  Changed to new-style prologue (RAL::KFH)
*           Replaced tabs in end-of-line comments (RAL::KFH)
*           Replaced fac_$name by fac1_name (RAL::KFH)
*           Inserted implicit none (RAL::KFH)
*    22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}
 
*  Notes:
*     Formerly known as MAG_$BLK
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MAG_SYS'         ! MAG Internal symbols and errors.
 
*  Global Variables:
      INCLUDE 'MAGIO_CMN'       ! MAG Initialisation Switch
*    Global data :
      DATA MAGINT/.FALSE./
 
*.
 
 
      END
