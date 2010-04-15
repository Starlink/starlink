      SUBROUTINE SPD_FDADR( XLOC, RNAME, ELEM, VALUE, STATUS )
*+
*  Name:
*     SPD_FDAD{CDIR}

*  Purpose:
*     Set a result extension vector element.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_FDADR( XLOC, RNAME, ELEM, VALUE, STATUS )

*  Description:
*     This routine sets the value of an element in a vector-shaped
*     extension to the result NDF in the Specdre Extension. The vector
*     must exist and be long enough.

*  Arguments:
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator of the Specdre Extension.
*     RNAME = CHARACTER * ( * ) (Given)
*        The name of the extension in the result NDF. I.e. the vector in
*        which an element is to be set.
*     ELEM = INTEGER (Given)
*        The element number to be set.
*     VALUE = REAL (Given)
*        The value to be stored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     02 Mar 1992 (hme):
*        Original version.
*     24 Nov 1994 (hme):
*        Renamed from SPAAXR
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Arguments Given:
      CHARACTER * ( * ) XLOC
      CHARACTER * ( * ) RNAME
      INTEGER ELEM
      REAL VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      CHARACTER * ( 7 ) COMPO    ! Structure name
      PARAMETER ( COMPO = 'RESULTS' )

*  Local Variables:
      INTEGER XNDF               ! Result NDF identifier
      CHARACTER * ( DAT__SZLOC ) RLOC ! Locator to vector
      CHARACTER * ( DAT__SZLOC ) TLOC ! Locator to vector element

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the element.
      CALL NDF_FIND( XLOC, COMPO, XNDF, STATUS )
      CALL NDF_XLOC( XNDF, RNAME, 'UPDATE', RLOC, STATUS )
      CALL DAT_CELL( RLOC, 1, ELEM, TLOC, STATUS )

*  Put the value.
      CALL DAT_PUT0R( TLOC, VALUE, STATUS )

*  Check status.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SPD_FDAD_NOACC',
     :      'SPD_FDAD: Error modifying extension vector element ' //
     :      'in result structure.', STATUS )
         GO TO 500
      END IF

*  Tidy up.
 500  CONTINUE
      CALL DAT_ANNUL( TLOC, STATUS )
      CALL DAT_ANNUL( RLOC, STATUS )
      CALL NDF_ANNUL( XNDF, STATUS )

*  Return.
      END
