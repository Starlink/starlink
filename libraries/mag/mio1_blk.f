      BLOCK DATA MIO1_BLK
*+
*  Name:
*     MIO1_BLK
 
*  Purpose:
*     MIO Block Data Initialisation.
 
*  Language:
*     Starlink Fortran
 
*  Invocation:
*     No invocation for Block Data
 
*  Description:
*     Initialise the MIOFIL Common Block so that implicit activation
*     of MIO can be done.
 
*  Authors:
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}
 
*  History:
*     3-Jul-1983:  Original.  (UCL::SLW)
*    15-Nov-1991:  Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*    22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}
 
*  Notes:
*     Formerly known as MIO_$BLK
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE
 
*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MIO_SYS'         ! MIO Symbolic Constants
 
*  Global Variables:
      INCLUDE 'MIOFIL_CMN'
*    Global data :
      DATA MIOINT/.FALSE./
 
*.
 
 
      END
