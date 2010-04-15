*+
      SUBROUTINE NDFX__SEEK_ITEM (NEL, FITSAR, ITEM, POS, STATUS)
*  Description:
*     Look for the position of a named FITS item in a FITS array
*  Invocation:
*     CALL NDFX__SEEK_ITEM (NEL, FITSAR, ITEM, POS, STATUS)
*  Arguments:
*     NEL = INTEGER (Given)
*        Number of element in FITS array
*     FITSAR = CHARACTER*80(NEL) (Given)
*        The FITS array
*     ITEM = CHARACTER*(*) (given)
*        Name of the FITS item to look for
*     POS = INTEGER (Returned)
*        Position of item in array
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Authors:
*     Alan Bridger (ab@jach.hawaii.edu)
*  History:
*     10-Apr-95: Original version (ab)
*  Bugs:
*     {note_any_bugs_here}
*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
*  Import:
      INTEGER  NEL               ! Number of elements
      CHARACTER*80 FITSAR(NEL)   ! FITS array
      CHARACTER*(*) ITEM         ! Name of ITEM
*  Export:
      INTEGER POS               ! Position of item in array
*  Status:
      INTEGER STATUS             ! Global status
*  External Functions:
      LOGICAL CHR_SIMLR
*  Local Variables:
      INTEGER I
      LOGICAL FOUND

*    Check the inherited global status.
      IF (STATUS .NE. SAI__OK) RETURN

*    Loop through looking for the item
      FOUND = .FALSE.
      I = 1
      DO WHILE (.NOT. FOUND .AND. I .LE. NEL)
         IF (CHR_SIMLR(ITEM, FITSAR(I)(1:8))) THEN
            FOUND = .TRUE.
            POS = I
         END IF
         I = I + 1
      END DO

      IF (.NOT. FOUND) STATUS = SAI__ERROR

*    If an error occurred, then report a message.
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP( ' ', 'Could not find FITS item', STATUS)
      END IF


      END
