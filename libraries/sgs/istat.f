      SUBROUTINE sgs_ISTAT (MODE, JSTAT)
*+
*   - - - - - -
*    I S T A T
*   - - - - - -
*
*   Initialise SGS status mode.
*
*
*   Given:
*      MODE      i     status mode:
*                                  0  = non-inherited
*                                  >0 = inherited
*                                  <0 = set to default (non-inherited) if not
*                                       already set
*
*      JSTAT     i     status (in inherited status mode)
*
*   Returned:
*      JSTAT     i     zero (in non-inherited status mode)
*
*   In inherited status mode SGS routines with a status argument only
*   execute if the status is set to zero on entry; in non-inherited
*   mode the status is ignored and the routines always execute.
*
*   Written to COMMON:
*      JSOPT     i     status mode
*
*   P.T.Wallace, D.L.Terrett   Starlink   13 January 1992
*-

      IMPLICIT NONE

      INTEGER MODE, JSTAT

      INCLUDE 'sgscom'


*   Flag to indicate whether the status mode has been set
      LOGICAL SET
      SAVE SET
      DATA SET/.FALSE./

      IF (MODE.GT.0) THEN

*     Exit immediately if error flagged
         IF (JSTAT.NE.0) GO TO 9999

*     Set inherited status mode
         JSOPT = 1

      ELSE IF (MODE.EQ.0) THEN

*     Disable inherited status mode
         JSOPT=0
      ELSE

*     Set default mode (disabled) if not already set
         IF (.NOT.SET) JSOPT=0
      END IF

*  Mode has now been set
      SET = .TRUE.

*  Return success
      JSTAT = 0

*  Exit
 9999 CONTINUE

      END
