*+ CGS3DR_SHOPAR - Show the current values of the CGS3 DR system parameters
      SUBROUTINE CGS3DR_SHOPAR (STATUS)
*    Description:
*     Show the values of the internal CGS3 DR system control parameters.
*    Invocation:
*     CALL CGS3DR_SHOPAR (STATUS)
*    Parameters:
*     STATUS = INTEGER (UPDATE)
*           ADAM Status return
*    Method:
*     If status bad then return
*     Show the current value of each parameter
*    Deficiencies:
*     None Known
*    Bugs:
*     None Known
*    Authors:
*     Alan Bridger (JAC::AB)
*    History:
*      9-Feb-93: Original (JAC::AB)
*     13-Jun-95: Add DIVBYSKY, CYCBYCYC, CYCBEG, CYCEND (JAC::AB)
*     15-Nov-95: Add ICHANBEG, ICHANEND, VERBOSE_PH (KK)
*    Type Definitions:
      IMPLICIT NONE
*    Global constants:
      INCLUDE 'SAE_PAR'
*    Import:
*    Import-Export:
*     None
*    Export:
*    Status:
      INTEGER STATUS
*    External references:
*    Global variables:
      INCLUDE 'CGS3DR_CMN'
*    Local Constants:
*     None
*    Local variables:
*    Internal References:
*     None
*    Local data:
*-

*    If status bad then return
      IF (STATUS .NE. SAI__OK) RETURN

*    Show the values of the control variables
      CALL MSG_SETL ('PL', PLOTTING)
      CALL MSG_OUT (' ','PLOTTING is currently ^PL', STATUS)
      CALL MSG_SETL ('VB', VERBOSE)
      CALL MSG_OUT (' ','VERBOSE is currently ^VB', STATUS)
      CALL MSG_SETL ('DB', DIVBYSKY)
      CALL MSG_OUT (' ', 'DIVBYSKY is currently ^DB', STATUS)
      CALL MSG_SETL ('DS', DIVBYSTD)
      CALL MSG_OUT (' ', 'DIVBYSTD is currently ^DS', STATUS)
      CALL MSG_SETL ('CB', CYCBYCYC)
      CALL MSG_OUT (' ', 'CYCBYCYC is currently ^CB', STATUS)
      CALL MSG_SETI ('CS', CYCBEG)
      CALL MSG_OUT (' ','CYCBEG is currently ^CS', STATUS)
      CALL MSG_SETI ('CE', CYCEND)
      CALL MSG_OUT (' ','CYCEND is currently ^CE', STATUS)
      CALL MSG_SETR ('NS', NSIGMA)
      CALL MSG_OUT (' ','NSIGMA is currently ^NS', STATUS)
      CALL MSG_SETC ('ST', STANDARD)
      CALL MSG_OUT (' ','STANDARD is currently ^ST', STATUS)
      CALL MSG_SETC ('GD', GDEVICE)
      CALL MSG_OUT (' ','GDEVICE is currently ^GD', STATUS)
      CALL MSG_SETI ('CHBE', ICHANBEG)
      CALL MSG_OUT (' ','ICHANBEG is currently ^CHBE', STATUS)
      CALL MSG_SETI ('CHND', ICHANEND)
      CALL MSG_OUT (' ','ICHANEND is currently ^CHND', STATUS)
      CALL MSG_SETL ('VERB', VERBOSE_PH)
      CALL MSG_OUT (' ','VERBOSE_PH is currently ^VERB', STATUS)

      END
