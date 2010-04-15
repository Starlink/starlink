*+ CGS3DR_INIT - Initiliase the CGS3 DR system
      SUBROUTINE CGS3DR_INIT (STATUS)
*    Description:
*     Initiliases the CGS3DR system. Get the date variables.
*     Get the names of the CGS3 monolith and Figaro task.
*    Invocation:
*     CALL CGS3DR_INIT (STATUS)
*    Parameters:
*     STATUS = INTEGER (UPDATE)
*           ADAM Status return
*    Method:
*     If status bad then return
*     Get the basic date from the parameter system
*     Construct the other date strings.
*     Get the names of the monoliths
*     Get the data directories
*    Deficiencies:
*     None Known
*    Bugs:
*     None Known
*    Authors:
*     Alan Bridger (JAC::AB)
*    History:
*     14-Dec-92: Original (JAC::AB)
*     15-Dec-92: Get data directories here (JAC::AB)
*      7-Jan-93: Add initialisation of control variables (JAC::AB)
*      8-Feb-93: Add NSIGMA (JAC::AB)
*     13-Jun-95: Add DIVBYSKY, CYCBYCYC, CYCBEG, CYCEND (JAC::AB)
*     15-Nov-95: Add ICHANBEG, ICHANEND, VERBOSE_PH (KK)
*     12-Dec-95: changed months to lower case
*      8-Mar-96: removed debug type statements
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
      INTEGER CHR_LEN                 ! Length of string
*    Global variables:
      INCLUDE 'CGS3DR_CMN'
*    Local Constants:
*     None
*    Local variables:
      INTEGER         LENGTH          ! Length of string
      INTEGER         IMONTH
      CHARACTER*3     MONTHS(12)
      CHARACTER*256   OUTVAL
*    Internal References:
*     None
*    Local data:
      DATA    MONTHS /'jan','feb','mar','apr','may','jun','jul','aug',
     : 'sep', 'oct', 'nov', 'dec'/
*-

*    If status bad then return
      IF (STATUS .NE. SAI__OK) RETURN

*    Get the basic date
      CALL PAR_GET0C ('DATE', UTDATE, STATUS)
      LENGTH = CHR_LEN(UTDATE)

      IF (LENGTH .EQ. 8) THEN
*       date is long form, create the other forms
         SUTDATE = UTDATE(3:8)
         READ (UTDATE(5:6), '(I2.2)') IMONTH
         UTDAYMONTH = UTDATE(7:8)//MONTHS(IMONTH)

      ELSE IF (LENGTH .EQ. 6) THEN
*       short form, create the others
         SUTDATE = UTDATE
         UTDATE = '19'//UTDATE
         READ (UTDATE(5:6), '(I2.2)') IMONTH
         UTDAYMONTH = UTDATE(7:8)//MONTHS(IMONTH)

      ELSE
*       date string must be wrong.
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'Date parameter has illegal format', STATUS)
      END IF

*    Get the monolith names
      CALL PAR_GET0C ('REDUCTION_TASK', REDUCTION_TASK, STATUS)
      CALL PAR_GET0C ('FIGARO_TASK', FIGARO_TASK, STATUS)
      CALL PAR_GET0C ('TSP_TASK', TSP_TASK, STATUS)

*    Get the data directories
      CALL PAR_GET0C ('DATADIR', DATADIR, STATUS)
      CALL PAR_GET0C ('RODIR', RODIR, STATUS)

*    Initialise control variables
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
*       Get the graphics device and open it
         CALL PAR_GET0C ('GDEVICE', GDEVICE, STATUS)
         CALL CGS3DR_OBEYW (FIGARO_TASK, 'SOFT', GDEVICE, OUTVAL,
     :    20000, STATUS)
      END IF


      END
