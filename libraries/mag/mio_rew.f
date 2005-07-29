 
      SUBROUTINE MIO_REW(TD, STATUS)
*+
*  Name:
*     MIO_REW
 
*  Purpose:
*     Rewind Tape.
 
*  Language:
*     Starlink Fortran
 
*  Invocation:
*     CALL MIO_REW(TD, STATUS)
 
*  Description:
*     Rewind the tape.
 
*  Arguments:
*     TD=INTEGER (Given)
*        A variable containing the tape descriptor.
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.
*        N.B. This routine does not report its own errors.
 
*  Algorithm:
*     Check for a valid tape descriptor and that the tape is open, if so, the
*     tape descriptor is used to get a device channel and the ioc_rew routine
*     is used to rewind the tape.
 
*  Authors:
*     Sid Wright (UCL::SLW)
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}
 
*  History:
*     06-Aug-1980: Original. (UCL::SLW)
*     01-FEB-1983:  Fortran 77 Version. (UCL::JRG)
*     10-May-1983: Tidy up for Starlink version. (UCL::SLW)
*     15-Jul-1986: Check return status of QIOW. (RAL::AJC)
*     15-Nov-1991:  Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*     17-Jan-1992: Changed to use ioc_rew instead of qiow (RAL::KFH)
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     {enter_further_changes_here}
 
*  Notes:
*     This is the Unix version.
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE
 
*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MIO_SYS'         ! MIO Internal Constants
      INCLUDE 'MIO_ERR'         ! MIO Errors
 
*  Arguments Given:
      INTEGER TD                ! tape descriptor
 
*  Status:
      INTEGER STATUS            ! status return
 
*  External References:
      EXTERNAL IOC_REW          ! rewind written in C
 
*  Local Variables:
      INTEGER MAGCN             ! channel number
 
*.
 
 
C      print *,'mio_rew:status,td',status,td
      IF ( STATUS.EQ.SAI__OK ) THEN
         CALL MIO1_CHAN(TD, MAGCN, STATUS)
         IF ( STATUS.EQ.SAI__OK ) CALL IOC_REW(MAGCN, STATUS)
      END IF
 
C      print *,'mio_rew:status',status
      END
