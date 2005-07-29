
      SUBROUTINE MIO_ACTIV(STATUS)
*+
*  Name:
*     MIO_ACTIV
 
*  Purpose:
*     initialise MIO system.
 
*  Language:
*     Starlink Fortran
 
*  Invocation:
*     CALL MIO_ACTIV(STATUS)
 
*  Description:
*     The MIO system is initialised.
 
*  Arguments:
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.
 
*  Algorithm:
*     The MIO error codes are given to SEM and the MIO common blocks are
*     initialised.
 
*  Authors:
*     Sid Wright  (UCL::SLW)
*     {enter_new_authors_here}
 
*  History:
*     17-Apr-1983:  Starlink Version. (UCL::SLW)
*     30-May-1986:  ADAM version   (RLVAD::AJC)
*     19-Sep-1986:  Remove external mio_exit  (RLVAD::AJC)
*     15-Nov-1991:  Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*           Removed commented-out code(RAL::KFH)
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE
 
*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
*    Status return :
      INTEGER STATUS            ! Status
 
*.
 
 
C      print *,'mio_activ:status', status
*    Execution allowed ?
      IF ( STATUS.NE.SAI__OK ) RETURN
 
*    Initialise common blocks
      CALL MIO_START(STATUS)
 
 
C      print *,'mio_activ:  completed'
 
      RETURN
      END
