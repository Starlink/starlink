      LOGICAL FUNCTION NUM_WASOK()
*+
*  Name:
*     NUM_WASOK

*  Purpose:
*     Check current numeric error status

*  Language:
*     Starlink Fortran

*  Invocation:
*     ISOK = NUM_WASOK()

*  Description:
*     Determine whether a numeric operation completed successfully.

*  Returned Value:
*     NUM_WASOK = LOGICAL
*        Returns TRUE if the numeric operation completed successfully.

*  Arguments:
*     STATUS = INTEGER (Given & Returned)
*        Current error status from NUM subsystem. It is not modified
*        if STATUS is not SAI__OK on entry.

*  Authors:
*     Tim Jenness (JAC, Hawaii)

*  History:
*     1-OCT-2004 (TIMJ):
*        Original version

*  Notes:
*     You should clear error status using NUM_CLEARERR before using
*     a NUM routine and using this routine to check status.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! For good status

*  Global Variables:
      INCLUDE 'NUM_CMN'          ! Define the NUM_ERROR flag

*.

      IF (NUM_ERROR .EQ. SAI__OK) THEN
         NUM_WASOK = .TRUE.
      ELSE
         NUM_WASOK = .FALSE.
      END IF

      END
