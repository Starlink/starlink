      BLOCK DATA MAG_BLK
*+
*  Name:
*     MAG_BLK
 
*  Purpose:
*     MAG Block Data Initialisation.
 
*  Language:
*     Starlink Fortran
 
*  Invocation:
*     No invocation for block data
 
*  Description:
*     Initialise the MAGGO Common Block so that implicit activation
*     of MAG can be done.
 
*  Authors:
*     Jack Giddings (UCL::JRG)
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}
 
*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     08-Nov-1991: (RAL::KFH)
*            Change to new style prologues
*            Change all fac_$name to fac1_name
*            Replace tabs in end-of-line comments
*            Remove /nolist in INCLUDE
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type definitions
      IMPLICIT NONE
 
*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MAG_SYS'         ! Mag Internal Symbolic Constants
 
*  Global Variables:
      INCLUDE 'MAGGO_SCL'       ! MAG Initialisation Switch
*    Global data :
      DATA MAGSLP/.TRUE./
 
*.
 
 
      END
