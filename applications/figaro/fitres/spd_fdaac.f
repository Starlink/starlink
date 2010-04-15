      SUBROUTINE SPD_FDAAC( ELEM1, ELEM2, ARRAY, VALUE, STATUS )
*+
*  Name:
*     SPD_FDAA{CDIR}

*  Purpose:
*     Fill part of an array with a constant value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FDAAC( ELEM1, ELEM2, ARRAY, VALUE, STATUS )

*  Description:
*     This routine fills part of an array with a constant value.
*     It is identical to an equivalent routine in UTIL, but copied here
*     in FITRES to make the library self-contained.

*  Arguments:
*     ELEM1 = INTEGER (Given)
*        The number of the first element in the array to be set.
*     ELEM2 = INTEGER (Given)
*        The number of the last element in the array to be set.
*     ARRAY( ELEM2 ) = CHARACTER * ( * ) (Returned)
*        The array to be filled.
*     VALUE = CHARACTER * ( * ) (Given)
*        The value to fill into the array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     27 Feb 1992 (hme):
*        Original version.
*     02 Apr 1992 (hme):
*        Silent return, if lower bound greater than upper bound.
*     25 Feb 1994 (hme):
*        Copy for use within FITRES library.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER ELEM1
      INTEGER ELEM2
      CHARACTER * ( * ) VALUE

*  Arguments Returned:
      CHARACTER * ( * ) ARRAY( ELEM2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index

*.

*  Check inherited global status, and if anything to do.
      IF ( STATUS .NE. SAI__OK .OR. ELEM1 .GT. ELEM2 ) RETURN

*  Loop through array.
      DO 1 I = ELEM1, ELEM2
         ARRAY(I) = VALUE
 1    CONTINUE

*  Return.
      END
