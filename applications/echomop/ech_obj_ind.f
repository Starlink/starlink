      INTEGER FUNCTION ECH_OBJ_IND( OBJECT_NAME )
*+
*  Name:
*     ECHOMOP - ECH_OBJ_IND

*  Purpose:
*     Get index of named object.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     24-APR-1996 (MJC):
*       Added prologue.
*     29-JUN-1996 (MJC):
*       Renamed function from ECH_MODULE_OBJECT_INDEX.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_MODULES.INC'
      INCLUDE 'ECH_MODLUT.INC'

*  Arguments Given:
      CHARACTER*( * ) OBJECT_NAME

*  Local Variables:
      INTEGER I
      INTEGER IST
      INTEGER IEN

      CHARACTER*80 WORK_STRING

*  Functions Called:
      INTEGER CHR_LEN
*.

*  Handle non-alpha first-character objects.
      IF ( ICHAR( OBJECT_NAME( :1 ) ) .LT. 65 ) THEN
         IST = REQ_OBJ_LUT( 27 )
         IEN = REQ_OBJ_LUT( 28 )

      ELSE
         I = ICHAR( OBJECT_NAME( :1 ) ) - 64
         IST = REQ_OBJ_LUT( I )
         IEN = REQ_OBJ_LUT( I + 1 )
      END IF
      ECH_OBJ_IND = MAX_REQUIRED_OBJECTS
      DO I = IST, IEN
         IF ( REQUIRED_OBJECTS( I ) .EQ. OBJECT_NAME ) THEN
            ECH_OBJ_IND = I
            GO TO 100
         END IF
      END DO

*  Failed to find the requested object.
      WORK_STRING = ' Illegal object: ' //
     :      OBJECT_NAME( :CHR_LEN( OBJECT_NAME ) ) // '.'
      CALL ECH_REPORT( 0, WORK_STRING )

 100  CONTINUE

      END
