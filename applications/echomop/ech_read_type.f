      SUBROUTINE ECH_READ_TYPE( OBJECT, LOC, IS_RDUCD, TYPE, STATUS )
*+
*  Name:
*     ECH_READ_TYPE

*  Purpose:
*     Determine data type of an object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ECH_READ_TYPE( OBJECT, LOC, IS_RDUCD, TYPE, STATUS )

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

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Arguments Given:
      CHARACTER*( * ) OBJECT
      CHARACTER*( * ) LOC
      LOGICAL IS_RDUCD

*  Arguments Returned
      CHARACTER*( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*( DAT__SZTYP ) LTYPE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Handle objects of known type in output files.
      IF ( IS_RDUCD ) THEN
         IF ( OBJECT .EQ. 'DATA_ARRAY' ) THEN
            TYPE = 'FLOAT'
            GO TO 900

         ELSE IF ( OBJECT .EQ. 'VARIANCE' ) THEN
            TYPE = 'FLOAT'
            GO TO 900

         ELSE IF ( OBJECT .EQ. 'XAXIS' ) THEN
            TYPE = 'FLOAT'
            GO TO 900

         ELSE IF ( OBJECT .EQ. 'YAXIS' ) THEN
            TYPE = 'INT'
            GO TO 900

         ELSE IF ( OBJECT .EQ. 'XLABEL' ) THEN
            TYPE = 'CHAR'
            GO TO 900

         ELSE IF ( OBJECT .EQ. 'YLABEL' ) THEN
            TYPE = 'CHAR'
            GO TO 900

         ELSE IF ( OBJECT .EQ. 'ZLABEL' ) THEN
            TYPE = 'CHAR'
            GO TO 900

         ELSE IF ( OBJECT .EQ. 'XUNITS' ) THEN
            TYPE = 'CHAR'
            GO TO 900

         ELSE IF ( OBJECT .EQ. 'YUNITS' ) THEN
            TYPE = 'CHAR'
            GO TO 900
         END IF
      END IF

*  Otherwise request object type.
      CALL CMP_TYPE( LOC, OBJECT, LTYPE, STATUS )

*  Translate to ECHOMOP-style names.
      IF ( LTYPE .EQ. '_BYTE' ) THEN
         TYPE = 'BYTE'

      ELSE IF ( LTYPE .EQ. '_UWORD' ) THEN
         TYPE = 'SHORT'

      ELSE IF ( LTYPE .EQ. '_INTEGER' ) THEN
         TYPE = 'INT'

      ELSE IF ( LTYPE .EQ. '_REAL' ) THEN
         TYPE = 'FLOAT'

      ELSE IF ( LTYPE .EQ. '_DOUBLE' ) THEN
         TYPE = 'DOUBLE'

      ELSE IF ( LTYPE( :5 ) .EQ. '_CHAR' ) THEN
         TYPE = 'CHAR'
      END IF

  900 CONTINUE

      END
