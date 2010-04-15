      SUBROUTINE JCMT_GETB( STRUCT, ELEMENT, ITEM, STATUS )
*+
*  Name:
*     JCMT_GETB

*  Purpose:
*     GET an item of type BYTE from DST file using DTA

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_GETB( STRUCT, ELEMENT, ITEM, STATUS )

*  Description:
*     Gets a single item from a DST file using the DTA library. It will
*     return an error message if it cannot find the item in the file.

*  Arguments:
*     STRUCT = CHARACTER * ( * ) (Given)
*        The structure holding the element
*     ELEMENT = CHARACTER * ( * ) (Given)
*        The element name within the structure
*     ITEM = BYTE (Returned)
*        The value of the item to be returned
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1990 (JBVAD::PAH):
*        Original version.
*     25-MAY-1991 (REVAD::JFL):
*        Made error message more informative
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  External functions:
      INTEGER ICH_LEN            ! Figaro string length function

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DTA_CODES'        ! DTA error codes

*  Arguments Given:
      CHARACTER * ( * ) STRUCT, ELEMENT

*  Arguments Returned:
      BYTE ITEM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGNORE             !
      CHARACTER * ( 128 ) DTA_NAME ! Full DTA name of the data item to
                                 ! be read

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL DTA_CRNAM(STRUCT, ELEMENT, 0, 0, DTA_NAME, STATUS)

*  read the value from the file
      CALL DTA_RDVARB(DTA_NAME, 1, ITEM, STATUS)
      IF ( STATUS .EQ. DTA_NOTFND ) THEN
         IGNORE = 0
         CALL PAR_WRUSER('Item '//DTA_NAME(:ICH_LEN(DTA_NAME))//
     :     ' not found', IGNORE)
      END IF



      END
      SUBROUTINE JCMT_GETC( STRUCT, ELEMENT, ITEM, STATUS )
*+
*  Name:
*     JCMT_GETC

*  Purpose:
*     GET an item of type CHARACTER from DST file using DTA

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_GETC( STRUCT, ELEMENT, ITEM, STATUS )

*  Description:
*     Gets a single item from a DST file using the DTA library. It will
*     return an error message if it cannot find the item in the file.

*  Arguments:
*     STRUCT = CHARACTER * ( * ) (Given)
*        The structure holding the element
*     ELEMENT = CHARACTER * ( * ) (Given)
*        The element name within the structure
*     ITEM = CHARACTER * ( * ) (Returned)
*        The value of the item to be returned
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1990 (JBVAD::PAH):
*        Original version.
*     25-MAY-1991 (REVAD::JFL):
*        Made error message more informative.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  External functions:
      INTEGER ICH_LEN            ! Figaro string length function

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DTA_CODES'        ! DTA error codes

*  Arguments Given:
      CHARACTER * ( * ) STRUCT, ELEMENT

*  Arguments Returned:
      CHARACTER*(*) ITEM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGNORE             !
      CHARACTER * ( 128 ) DTA_NAME ! Full DTA name of the data item to
                                 ! be read

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL DTA_CRNAM(STRUCT, ELEMENT, 0, 0, DTA_NAME, STATUS)

*  read the value from the file
      CALL DTA_RDVARC(DTA_NAME, 1, ITEM, STATUS)
      IF ( STATUS .EQ. DTA_NOTFND ) THEN
         IGNORE = 0
         CALL PAR_WRUSER('Item '//DTA_NAME(:ICH_LEN(DTA_NAME))//
     :     ' not found', IGNORE)
      END IF



      END
      SUBROUTINE JCMT_GETD( STRUCT, ELEMENT, ITEM, STATUS )
*+
*  Name:
*     JCMT_GETD

*  Purpose:
*     GET an item of type DOUBLE from DST file using DTA

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_GETD( STRUCT, ELEMENT, ITEM, STATUS )

*  Description:
*     Gets a single item from a DST file using the DTA library. It will
*     return an error message if it cannot find the item in the file.

*  Arguments:
*     STRUCT = CHARACTER * ( * ) (Given)
*        The structure holding the element
*     ELEMENT = CHARACTER * ( * ) (Given)
*        The element name within the structure
*     ITEM = DOUBLE PRECISION (Returned)
*        The value of the item to be returned
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1990 (JBVAD::PAH):
*        Original version.
*     25-MAY-1991 (REVAD::JFL):
*        Made error message more informative
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  External functions:
      INTEGER ICH_LEN            ! Figaro string length function

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DTA_CODES'        ! DTA error codes

*  Arguments Given:
      CHARACTER * ( * ) STRUCT, ELEMENT

*  Arguments Returned:
      DOUBLE PRECISION ITEM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGNORE             !
      CHARACTER * ( 128 ) DTA_NAME ! Full DTA name of the data item to
                                 ! be read

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL DTA_CRNAM(STRUCT, ELEMENT, 0, 0, DTA_NAME, STATUS)

*  read the value from the file
      CALL DTA_RDVARD(DTA_NAME, 1, ITEM, STATUS)
      IF ( STATUS .EQ. DTA_NOTFND ) THEN
         IGNORE = 0
         CALL PAR_WRUSER('Item '//DTA_NAME(:ICH_LEN(DTA_NAME))//
     :     ' not found', IGNORE)
      END IF

      END



      SUBROUTINE JCMT_GETF( STRUCT, ELEMENT, ITEM, STATUS )
*+
*  Name:
*     JCMT_GETF

*  Purpose:
*     GET an item of type REAL from DST file using DTA

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_GETF( STRUCT, ELEMENT, ITEM, STATUS )

*  Description:
*     Gets a single item from a DST file using the DTA library. It will
*     return an error message if it cannot find the item in the file.

*  Arguments:
*     STRUCT = CHARACTER * ( * ) (Given)
*        The structure holding the element
*     ELEMENT = CHARACTER * ( * ) (Given)
*        The element name within the structure
*     ITEM = REAL (Returned)
*        The value of the item to be returned
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1990 (JBVAD::PAH):
*        Original version.
*     25-MAY-1991 (REVAD::JFL):
*        Made error message more informative
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  External functions:
      INTEGER ICH_LEN            ! Figaro string length function

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DTA_CODES'        ! DTA error codes

*  Arguments Given:
      CHARACTER * ( * ) STRUCT, ELEMENT

*  Arguments Returned:
      REAL ITEM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGNORE             !
      CHARACTER * ( 128 ) DTA_NAME ! Full DTA name of the data item to
                                 ! be read

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL DTA_CRNAM(STRUCT, ELEMENT, 0, 0, DTA_NAME, STATUS)

*  read the value from the file
      CALL DTA_RDVARF(DTA_NAME, 1, ITEM, STATUS)
      IF ( STATUS .EQ. DTA_NOTFND ) THEN
         IGNORE = 0
         CALL PAR_WRUSER('Item '//DTA_NAME(:ICH_LEN(DTA_NAME))//
     :     ' not found', IGNORE)
      END IF

      END



      SUBROUTINE JCMT_GETI( STRUCT, ELEMENT, ITEM, STATUS )
*+
*  Name:
*     JCMT_GETI

*  Purpose:
*     GET an item of type INTEGER from DST file using DTA

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_GETI( STRUCT, ELEMENT, ITEM, STATUS )

*  Description:
*     Gets a single item from a DST file using the DTA library. It will
*     return an error message if it cannot find the item in the file.

*  Arguments:
*     STRUCT = CHARACTER * ( * ) (Given)
*        The structure holding the element
*     ELEMENT = CHARACTER * ( * ) (Given)
*        The element name within the structure
*     ITEM = INTEGER (Returned)
*        The value of the item to be returned
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1990 (JBVAD::PAH):
*        Original version.
*     25-MAY-1991 (REVAD::JFL):
*        Made error message more informative
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  External functions:
      INTEGER ICH_LEN            ! Figaro string length function

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DTA_CODES'        ! DTA error codes

*  Arguments Given:
      CHARACTER * ( * ) STRUCT, ELEMENT

*  Arguments Returned:
      INTEGER ITEM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IGNORE             !
      CHARACTER * ( 128 ) DTA_NAME ! Full DTA name of the data item to
                                 ! be read

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL DTA_CRNAM(STRUCT, ELEMENT, 0, 0, DTA_NAME, STATUS)

*  read the value from the file
      CALL DTA_RDVARI(DTA_NAME, 1, ITEM, STATUS)
      IF ( STATUS .EQ. DTA_NOTFND ) THEN
         IGNORE = 0
         CALL PAR_WRUSER('Item '//DTA_NAME(:ICH_LEN(DTA_NAME))//
     :     ' not found', IGNORE)
      END IF

      END
