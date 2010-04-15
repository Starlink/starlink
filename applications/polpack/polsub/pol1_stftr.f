      SUBROUTINE POL1_STFTR( NCARD, FITS, VAL, FTNAM, ICARD, COMMNT,
     :                       NEW, STATUS )
*+
*  Name:
*     POL1_STFTR

*  Purpose:
*     Store a given real value as a FITS card.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_STFTR( NCARD, FITS, VAL, FTNAM, ICARD, COMMNT, NEW, STATUS )

*  Description:
*     This routine stores the supplied real value as a FITS header card
*     using the keyword name given by FTNAM, in an array of character strings.
*
*     If the keyword already exists in the FITS array, then its value is
*     replaced. Otherwise, the new card is stored at index ICARD.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of cards to be searched in the FITS array.
*     FITS( * ) = CHARACTER * ( * ) (Given)
*        The array holding the FITS cards.
*     VAL = REAL (Given)
*        The value to store.
*     FTNAM = CHARACTER * ( * ) (Given)
*        The name of the FITS keyword to be used.
*     ICARD = INTEGER (Given)
*        The index within FITS at which to store the named component, if
*        an existing card for the keyword cannot be found.
*     COMMNT = CHARACTER * ( * ) (Given)
*        The comment to include in the FITS header card.
*     NEW = LOGICAL (Returned)
*        Was no existing card found for the specified keyword?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-APR-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NCARD
      CHARACTER FITS( * )*(*)
      REAL VAL
      CHARACTER FTNAM*(*)
      INTEGER ICARD
      CHARACTER COMMNT*(*)

*  Arguments Returned:
      LOGICAL NEW

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CVAL*80          ! Existing card's value
      CHARACTER COM*80           ! Existing card's comment
      INTEGER JCARD              ! Index at which to store the card
      LOGICAL THERE              ! Does card already exist?
*.

      NEW = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the keyword already exists in the FITS array.
      CALL FTS1_GKEYC( NCARD, FITS, 1, FTNAM, 1, THERE, CVAL, COM,
     :                 JCARD, STATUS )

*  If not, create a new card and store it at the supplied index. Otherwise
*  store it at the index of the existing card.
      IF( .NOT. THERE ) THEN
         JCARD = ICARD
         NEW = .TRUE.
      ELSE
         NEW = .FALSE.
      END IF

*  Format the supplied value as a FITS card using the supplied FITS keyword
*  name.
      CALL FTS1_WKEYR( FTNAM, VAL, '/', COMMNT, FITS( JCARD ), STATUS )

      END
