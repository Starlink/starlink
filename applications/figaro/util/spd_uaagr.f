      REAL FUNCTION SPD_UAAGR( ARRAY, ELEM, STATUS )
*+
*  Name:
*     SPD_UAAG{CDIR}

*  Purpose:
*     Get an array element.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SPD_UAAGR( ARRAY, ELEM, STATUS )

*  Description:
*     This function returns one element from an array. This is
*     particularly useful when only few elements from the array are
*     needed and when the array is known only by a pointer.

*  Arguments:
*     ARRAY( ELEM ) = REAL (Given)
*        The array from which an element is to be returned. Its actual
*        length will usually be larger.
*     ELEM = INTEGER (Given)
*        The number of the element to be returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     SPD_UAAGR = REAL
*        The returned value is simply the ELEM-th element picked from
*        ARRAY.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     27 Feb 1992 (hme):
*        Original version.
*     02 Apr 1992 (hme):
*        C-flavour.
*     19 May 1994 (hme):
*        After having copied this to FITRES, not copy it on to UTIL.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER ELEM
      REAL ARRAY( ELEM )

*  Status:
      INTEGER STATUS             ! Global status

*.

      IF ( STATUS .NE. SAI__OK ) RETURN
      SPD_UAAGR = ARRAY( ELEM )
      END
