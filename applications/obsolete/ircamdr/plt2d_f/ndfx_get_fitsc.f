*+
      SUBROUTINE NDFX_GET_FITSC (NDF, ITEM, VALUE, STATUS)
*  Description:
*     Get a named FITS item from an NDF into a CHARACTER value
*  Invocation:
*     CALL NDFX_GET_FITSC (NDF, ITEM, VALUE, STATUS)
*  Arguments:
*     NDF = INTEGER (Given)
*        NDF identifier to the NDF for which a FITS item is desired
*     ITEM = CHARACTER*(*) (Given)
*        Name of the FITS item for which a value is desired
*     VALUE = CHARACTER*(*) (Returned)
*        Value of the item
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Authors:
*     Alan Bridger (ab@jach.hawaii.edu)
*  History:
*     10-Apr-95: Original version (ab)
*     11-Apr-95: Add the LENGTH argument for the Unix version  (ab)
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
      INTEGER  NDF               ! NDF identifier
      CHARACTER*(*) ITEM         ! Name of FITS item requested
*  Export:
      CHARACTER*(*) VALUE        ! Value to be returned
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  EL,                      ! Number of cards in the header
     :  POS,                     ! position of FITS item in array
     :  LENGTH,                  ! Dummy parameter to fool unix
     :  PNTR(1)                  ! Pointer to the card images
      CHARACTER*(DAT__SZLOC)
     :  LOC                      ! Locator to the FITS extension.


*    Check the inherited global status.
      IF (STATUS .NE. SAI__OK) RETURN

*    Obtain a locator to the FITS extension.
      CALL NDF_XLOC (NDF, 'FITS', 'READ', LOC, STATUS)

*    Map the 80-character FITS card images stored within the extension.
      CALL DAT_MAPV (LOC, '_CHAR*80', 'READ', PNTR(1), EL, STATUS)
      LENGTH = 80

*    Check the status before accessing EL.
      IF (STATUS .EQ. SAI__OK) THEN

*       Loop through the headers looking for one that matches "ITEM".
         CALL NDFX__SEEK_ITEM (EL, %val(PNTR(1)), ITEM, POS, STATUS,
     :   %val(LENGTH))

*       If no error then get the value
         CALL NDFX__GET_FITSC (EL, %val(PNTR(1)), POS, VALUE, STATUS,
     :   %val(LENGTH))

      END IF

*    Tidy the locator to the FITS extension.
      CALL DAT_ANNUL (LOC, STATUS)

*    If an error occurred, then report a message.
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP( ' ', 'Error getting a FITS item', STATUS)
      END IF


      END
