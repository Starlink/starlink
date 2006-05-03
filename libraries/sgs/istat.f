      SUBROUTINE sgs_ISTAT (MODE, JSTAT)
*+
*  Name:
*     ISTAT

*  Purpose:
*     Initialise SGS status mode.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     MODE = INTEGER (Given)
*         Status Mode:
*            0  = non-inherited
*            >0 = inherited
*            <0 = set to default (non-inherited) if not
*                 already set
*     JSTAT = INTEGER (Given & Returned)
*         Status in inherited status mode.
*         Returns zero in non-inherited status mode.

*  Description:
*     In inherited status mode SGS routines with a status argument only
*     execute if the status is set to zero on entry; in non-inherited
*     mode the status is ignored and the routines always execute.

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1992 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Written To Common:
*     JSOPT     i     status mode

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
