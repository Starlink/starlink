      SUBROUTINE CON_MOVE (NBYTES, FROM, TO, STATUS)
*+
*  Name:
*     CON_MOVE

*  Purpose:
*     Copies bytes of data from one location to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_MOVE (NBYTES, FROM, TO, STATUS)

*  Description:
*     Simply moves bytes of data from one array to another.

*  Arguments:

*     NBYTES = INTEGER (Given)
*        Number of bytes to be moved.
*     FROM(NBYTES) = BYTE (Given)
*        Source array for data.
*     TO (NBYTES) = BYTE (Returned)
*        Destination array for data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     JM: Jo Murray (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 Feb 8 (JM):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT    NONE           ! No implicit typing

*  Global Constants:
      INCLUDE    'SAE_PAR'       ! Standard SAE constants

*  Arguments Given:
      INTEGER    NBYTES          ! Number of bytes to be moved.
      BYTE       FROM(NBYTES)    ! Source array.

*  Arguments Reurned:
      BYTE       TO(NBYTES)      ! Destination array.

*  Status:
      INTEGER STATUS             ! Global status

*  Local variable:
      INTEGER    I               ! Loop variable
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Move the bytes.
      DO I=1, NBYTES
         TO(I) = FROM(I)
      END DO
 
      END
