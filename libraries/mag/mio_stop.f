      SUBROUTINE MIO_STOP(STATUS)
*+
*  Name:
*     MIO_STOP
 
*  Purpose:
*     Close down MIO.
 
*  Language:
*     Starlink Fortran
 
*  Invocation:
*     CALL MIO_STOP(STATUS)
 
*  Description:
*     Close all open files.
 
*  Arguments:
*     STATUS=INTEGER (Given and Returned)
*        Variable holding the status value.   If the routine fails to
*        complete, this variable will be set to an appropriate error number.
*        If this variable is not SAI__OK on input, then the routine will
*        still attempt to execute, but will return with STATUS set to the
*        import value.
*        N.B. This routine does not report its own errors.
 
*  Algorithm:
*     The MIO system is run down by successive calls to MIO_CLOSE for each
*     active tape descriptor.
 
*  Authors:
*     Sid Wright (UCL::SLW)
*     Jon Fairclough (RAL::IPMAF)
*     {enter_new_authors_here}
 
*  History:
*     30-Jul-1980: Original. (UCL::SLW)
*     10-May-1983: Tidy up for Starlink version. (UCL::SLW)
*        3-Dec-1984: Mioint flag set. (RAL::IPMAF)
*     15-Nov-1991:  Changed to new style prologue (RAL::KFH)
*           Replaced tabs by spaces in end-of-line comments (RAL::KFH)
*           Changed any fac_$name into fac1_name (RAL::KFH)
*           Inserted IMPLICIT NONE (RAL::KFH)
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE
 
*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MIO_SYS'         ! MIO Internal symbols and errors.
 
*  Arguments Returned:
 
*  Status:
      INTEGER STATUS            ! status return
 
*  External References:
      EXTERNAL MIO1_BLK          ! Block data subprogram that
                                 ! initializes MIOINT
*  Global Variables:
      INCLUDE 'MIOFIL_CMN'
 
*  Local Variables:
      INTEGER ISTAT
      INTEGER I                 ! loop index
 
*.
 
 
      IF ( .NOT.MIOINT ) RETURN
*
      ISTAT = STATUS
      STATUS = SAI__OK
      DO 100 I = 1, MIO__MXDEV
         IF ( .NOT.MFREE(I) ) CALL MIO_CLOSE(I, STATUS)
 100  CONTINUE
      MIOINT = .FALSE.
 
      IF ( ISTAT.NE.SAI__OK ) STATUS = ISTAT
 
      RETURN
      END
