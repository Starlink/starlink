*+
      SUBROUTINE NDFX__GET_FITSC (NEL, FITSAR, POS, VALUE, STATUS)
*  Description:
*     Return the value of character FITS item at position POS in the FITS array.
*  Invocation:
*     CALL NDFX__GET_FITSC (NEL, FITSAR, POS, VALUE, STATUS)
*  Arguments:
*     NEL = INTEGER (Given)
*        Number of element in FITS array
*     FITSAR = CHARACTER*80(NEL) (Given)
*        The FITS array
*     POS = INTEGER (Given)
*        Position for which value is required
*     VALUE = REAL (Returned)
*        Value of the item
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
      INTEGER  POS              ! Position in array
*  Export:
      CHARACTER*(*) VALUE                 ! Value to return
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER POS1, POS2

*    Check the inherited global status.
      IF (STATUS .NE. SAI__OK) RETURN

*    Extract the value from the relevant field
      CALL CHR_DELIM (FITSAR(POS)(10:30), CHAR(39), POS1, POS2)
      VALUE = FITSAR(POS)(POS1+10:POS2+8)

*    If an error occurred, then report a message.
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP( ' ', 'Error translating the value of a '/
     :    /'FITS item', STATUS)
      END IF


      END
