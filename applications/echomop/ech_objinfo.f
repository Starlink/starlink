      SUBROUTINE ECH_OBJINFO( REQUIRED_OBJECT, ADDRESS, TYPE, SIZE,
     :                        HANDLE, ACTIVE )
*+
*  Name:
*     ECHOMOP - ECH_OBJINFO

*  Purpose:
*     Search object dictionary.

*  Description:
*    This routine searches the object dictionary and returns the properties
*    of the named object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ECH_OBJINFO( REQUIRED_OBJECT, ADDRESS, TYPE, SIZE,
*    :                  HANDLE, ACTIVE )

*  Arguments:
*     REQUIRED_OBJECT = CHAR (Given)
*        Internal reference name of object.
*     ADDRESS = INTEGER (Returned)
*        Adress of a mapped array.
*     TYPE = CHAR (Returned)
*        Type of fitting function.
*     SIZE = INTEGER (Returned)
*        Size of object.
*     HANDLE = INTEGER (Returned)
*        Data access library pointer for object.
*     ACTIVE = LOGICAL (Returned)
*        TRUE if object active and has value.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     08-APR-1996 (MJC):
*       Standard Prologue.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_USE_RDCTN.INC'

*  Arguments Given:
      CHARACTER*( * ) REQUIRED_OBJECT

*  Arguments Returned:
      CHARACTER*( * ) TYPE
      INTEGER SIZE
      INTEGER ADDRESS
      INTEGER HANDLE
      LOGICAL ACTIVE

*  Local variables:
      INTEGER I
*.

*  Look up entry in table.
      ACTIVE = .FALSE.
      DO I = 1, ACCESS_COUNT

*     Return the details of this object when found.
         IF ( OBJECT_NAME( I ) .EQ. REQUIRED_OBJECT ) THEN
            ACTIVE = .TRUE.
            ADDRESS = OBJECT_ADDRESS( I )
            HANDLE = OBJECT_HANDLE( I )
            SIZE = OBJECT_SIZE( I )
            TYPE = OBJECT_TYPE( I )
            GO TO 999
         END IF
      END DO
 999  CONTINUE

      END
