 
      SUBROUTINE MAG1_ERRTP(TP, STATUS)
*+
*  Name:
*     MAG1_ERRTP
 
*  Purpose:
*     report MAG error message to STARLINK environment.
 
*  Language:
*     Starlink Fortran
 
*  Invocation:
*     CALL MAG1_ERRTP(TP, STATUS)
 
*  Description:
*     An error is reported, based on the supplied tape descriptor and
*     MAG status value.
 
*  Arguments:
*     TP=INTEGER (Given)
*        A variable containing the tape descriptor
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If this variable is not
*        SAI__OK on input, then the routine will return without action.  If
*        the routine fails to complete, this variable will be set to an
*        appropriate error number.
 
*  Algorithm:
*     Use MAG1_DEV to get the device name associated with the tape
*     descriptor, and then use the MSG and ERR systems.
 
*  Authors:
*     Jack Giddings (UCL::JRG)
*     Sid Wright (UCL::SLW)
*     {enter_new_authors_here}
 
*  History:
*     11-SEP-1981:  Original.  (UCL::JRG)
*     01-FEB-1983:  Fortran 77 Version. (UCL::JRG)
*     14-Jul-1983:  Re-organised version. (UCL::SLW)
*     14-Nov-1991:  Changed to new-style prologue (RAL::KFH)
*           Replaced tabs in end-of-line comments (RAL::KFH)
*           Replaced fac_$name by fac1_name (RAL::KFH)
*           Inserted implicit none (RAL::KFH)
*     22-Jan-1993:  Change include file names
*           Convert code to uppercase using SPAG (RAL::BKM)
*     29-Jun-1995:  Use FACER not ^STATUS (RAL::AJC)
*     {enter_further_changes_here}
 
*  Notes:
*     Formerly known as MAG_$ERRTP
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type definition:
      IMPLICIT NONE
 
*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MAG_SYS'         ! MAG Internal Constants
 
*  Arguments Given:
      INTEGER TP                ! tape descriptor
      INTEGER STATUS            ! status code
 
*  External References:
      LOGICAL MAG1_DEV          ! Get device parameter from descriptor
 
*  Local Variables:
      CHARACTER*(MAG__SZNAM) DEVICE   ! Device parameter name
*.
 
      CALL MAG1_CODE(STATUS)

*    Attempt to get name of device associated with descriptor
      IF ( .NOT.MAG1_DEV(TP,DEVICE) ) THEN
         CALL MSG_SETI('DSC', TP)
         CALL ERR_FACER('STAT', STATUS)
         CALL ERR_REP('MAG_INVDSC', '^DSC : ^STAT', STATUS)
      ELSE
         CALL MSG_SETC('MAG', DEVICE)
         CALL ERR_FACER('STAT', STATUS)
         CALL ERR_REP('MAG_ERNAM', '^MAG : ^STAT', STATUS)
      END IF
 
      RETURN
      END
