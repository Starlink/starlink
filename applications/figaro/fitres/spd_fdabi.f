      INTEGER FUNCTION SPD_FDABI( ARRAY, ELEM, STATUS )
*+
*  Name:
*     SPD_FDAB{CDIR}

*  Purpose:
*     Get an array element.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SPD_FDABI( ARRAY, ELEM, STATUS )

*  Description:
*     This function returns one element from an array. This is
*     particularly useful when only few elements from the array are
*     needed and when the array is known only by a pointer.
*
*     This routine is a copy of the same routine in UTIL. A copy exists
*     here to make the FITRES library self-contained.

*  Arguments:
*     ARRAY( ELEM ) = INTEGER (Given)
*        The array from which an element is to be returned. Its actual
*        length will usually be larger.
*     ELEM = INTEGER (Given)
*        The number of the element to be returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     SPD_FDABI = INTEGER
*        The returned value is simply the ELEM-th element picked from
*        ARRAY.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     27-FEB-1992 (HME):
*        Original version.
*     02-APR-1992 (HME):
*        C-flavour.
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
      INTEGER ARRAY( ELEM )

*  Status:
      INTEGER STATUS             ! Global status

*.

      IF ( STATUS .NE. SAI__OK ) RETURN
      SPD_FDABI = ARRAY( ELEM )
      END
