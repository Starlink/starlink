      SUBROUTINE NUM_GETERR( STATUS )
*+
*  Name:
*     NUM_GETERR

*  Purpose:
*     Obtain the current numeric error status

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL NUM_GETERR( STATUS )

*  Description:
*     Obtain the current numerical processing error status and
*     place it in the inherited status variable.

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
*     If you are only interested in checking whether a numeric operation
*     succeeded, you should use NUM_ISOK().

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! For good status

*  Global Variables:
      INCLUDE 'NUM_CMN'          ! Define the NUM_ERROR flag

*  Inherited Status:
      INTEGER STATUS

*.

*     Do nothing if already bad status
      IF (STATUS .NE. SAI__OK) RETURN

      STATUS = NUM_ERROR

      END
