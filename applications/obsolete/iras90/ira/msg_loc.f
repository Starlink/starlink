      SUBROUTINE MSG_LOC( TOKEN, LOC )
*+
*  Name:
*     MSG_LOC

*  Purpose:
*     Assign an object name to a message token.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG_LOC( TOKEN, LOC )

*  Description:
*     This routine provides an alternative to the obsolete MSG_LOC
*     routine within the MSG library. The routine is used by the
*     TRANSFORM system, but is only available in the ADAM version of the
*     MSG library, thus making it difficult to link stand-alone
*     TRANSFORM applications. This version puts the total object path
*     (obtained using HDS_TRACE) into the token (the original included
*     the parameter name within the token).

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        The name of the MSG token.
*     LOC = CHARACTER * ( * ) (Given)
*        HDS locator to the data object.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'DAT_ERR'          ! DAT error constants

*  Arguments Given:
      CHARACTER * ( * ) TOKEN
      CHARACTER * ( * ) LOC

*  Local Variables:
      CHARACTER CVAL*1           ! Dummy character argument.
      CHARACTER PATH*255         ! Path to data object.

      INTEGER NLEV               ! No. of nodes within path.
      INTEGER STATUS             ! Local status value.

*.

*  Initialise an error reporting context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Get the path for the object.
      CALL HDS_TRACE( LOC, NLEV, PATH, CVAL, STATUS )

*  If a text string was truncated, annul the error.
      IF( STATUS .EQ. DAT__TRUNC ) CALL ERR_ANNUL( STATUS )

*  If no error occurred, assign the path to the token.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETC( TOKEN, PATH )

*  Otherwise, flush the error and set the token blank.
      ELSE
         CALL ERR_FLUSH( STATUS )
         CALL MSG_SETC( TOKEN, ' ' )
      END IF

*  End the error reporting context.
      CALL ERR_RLSE

      END
