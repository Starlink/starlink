      SUBROUTINE ECH_GET_DIMS( OBJECT, DIMS, IDOT, ILLEN, IEND, IDIM )
*+
*  Name:
*     ECH_GET_DIMS

*  Purpose:
*     Determine dimensions of an object.

*  Arguments:
*     OBJECT = CHARACTER*( * ) (Given)
*        Name of the object to be measured.

*  Authors:
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     20-MAR-1997 (MJC):
*       Added prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Defintions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'ECH_USE_RDCTN.INC'

*  Arguments:
      CHARACTER*( * ) OBJECT
      INTEGER DIMS( * )
      INTEGER IDOT
      INTEGER ILLEN
      INTEGER IEND
      INTEGER IDIM

*  Local Variables:
      INTEGER I
      INTEGER J
      INTEGER IBEG
      INTEGER ISEP
      INTEGER ILEN
      INTEGER STATUS
*.
      STATUS = SAI__OK
      DO J = 1, MAX_DIMENSIONS
         DIMS( J ) = 0
      END DO

*  Initialise pointers into string.
      IBEG = 0
      IDIM = 0
      ISEP = 0
      IEND = ILLEN

*  Look for object dimensions in string.
      I = IDOT + 1
      DO WHILE ( I .LE. ILLEN )
         IF ( OBJECT( I:I ) .EQ. '[' ) THEN
            IEND = I - 1
            IBEG = I + 1
         END IF
         IF ( OBJECT( I:I ) .EQ. ',' ) ISEP = I
         IF ( OBJECT( I:I ) .EQ. ']' ) ISEP = I
         IF ( ISEP .GT. IBEG ) THEN
            IDIM = IDIM + 1
            ILEN = ISEP - IBEG

*        Read object dimensions from string.
            CALL CHR_CTOI( OBJECT( IBEG:ISEP-1 ), DIMS( IDIM ), STATUS )
         IBEG = ISEP + 1
         END IF
         I = I + 1
      END DO

      END
