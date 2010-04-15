*+ CGS3DR_SETPAR - Modify a parameter in the CGS3 DR system
      SUBROUTINE CGS3DR_SETPAR (STATUS)
*    Description:
*     Changes the value of an internal CGS3 DR system control parameter.
*    Invocation:
*     CALL CGS3DR_SETPAR (STATUS)
*    Parameters:
*     STATUS = INTEGER (UPDATE)
*           ADAM Status return
*    Method:
*     If status bad then return
*     Get the latest value of each parameter
*    Deficiencies:
*     None Known
*    Bugs:
*     None Known
*    Authors:
*     Alan Bridger (JAC::AB)
*    History:
*      9-Feb-93: Original (JAC::AB)
*     13-Jun-95: Add DIVBYSKY, CYCBYCYC, CYCBEG, CYCEND (JAC::AB)
*     16-Nov-95: Add ICHANBEG, ICHANEND, VERBOSE_PH
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
*      INTEGER CHR_LEN                 ! Length of string
*    Global variables:
      INCLUDE 'CGS3DR_CMN'
*    Local Constants:
*     None
*    Local variables:
      CHARACTER*20    NEWDEVICE       ! NEW Graphics device
      CHARACTER*256   OUTVAL
*    Internal References:
*     None
*    Local data:
*-

*    If status bad then return
      IF (STATUS .NE. SAI__OK) RETURN

*    Get values of the control variables
      CALL PAR_GET0L ('PLOTTING', PLOTTING, STATUS)
      CALL PAR_GET0L ('VERBOSE', VERBOSE, STATUS)
      CALL PAR_GET0L ('DIVBYSKY', DIVBYSKY, STATUS)
      CALL PAR_GET0L ('DIVBYSTD', DIVBYSTD, STATUS)
      CALL PAR_GET0L ('CYCBYCYC', CYCBYCYC, STATUS)
      CALL PAR_GET0I ('CYCBEG', CYCBEG, STATUS)
      CALL PAR_GET0I ('CYCEND', CYCEND, STATUS)
      CALL PAR_GET0R ('NSIGMA', NSIGMA, STATUS)
      CALL PAR_GET0C ('STANDARD', STANDARD, STATUS)
      CALL PAR_GET0I ('ICHANBEG', ICHANBEG, STATUS)
      CALL PAR_GET0I ('ICHANEND', ICHANEND, STATUS)
      CALL PAR_GET0L ('VERBOSE_PH', VERBOSE_PH, STATUS)

      IF (PLOTTING) THEN
*       Get the graphics device and, if different from its last value,
*       open it
         CALL PAR_GET0C ('GDEVICE', NEWDEVICE, STATUS)
         IF (NEWDEVICE .NE. GDEVICE) THEN
            GDEVICE = NEWDEVICE
            CALL CGS3DR_OBEYW (FIGARO_TASK, 'SOFT', GDEVICE, OUTVAL,
     :       20000, STATUS)
         END IF
      END IF

*    cancel the parameters
      CALL PAR_CANCL ('PLOTTING', STATUS)
      CALL PAR_CANCL ('VERBOSE', STATUS)
      CALL PAR_CANCL ('DIVBYSKY', STATUS)
      CALL PAR_CANCL ('DIVBYSTD', STATUS)
      CALL PAR_CANCL ('CYCBYCYC', STATUS)
      CALL PAR_CANCL ('CYCBEG', STATUS)
      CALL PAR_CANCL ('CYCEND', STATUS)
      CALL PAR_CANCL ('NSIGMA', STATUS)
      CALL PAR_CANCL ('STANDARD', STATUS)
      CALL PAR_CANCL ('GDEVICE', STATUS)
      CALL PAR_CANCL ('ICHANBEG', STATUS)
      CALL PAR_CANCL ('ICHANEND', STATUS)
      CALL PAR_CANCL ('VERBOSE_PH', STATUS)

*    display the results
      CALL CGS3DR_SHOPAR (STATUS)


      END
