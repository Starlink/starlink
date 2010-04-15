      SUBROUTINE MV4_MAPCLS( IFAIL )
*+
*  Name:
*     MV4_MAPCLS

*  Purpose:
*     Close the map file for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MV4_MAPCLS( IFAIL )

* Description:
*     This routine closes the map file for Specx.

*  Arguments:
*     IFAIL = INTEGER (Given and Returned)
*        Returned non-zero on failure.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     12 Aug 1994 (hme):
*        Original version.
*     31 Aug 1994 (hme):
*        NDF/HDS-based sparse cube.
*     12 Oct 1994 (hme):
*        Return IFAIL 18 instead of an arbitrary Starlink error.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants

*  Global Variables:
      INCLUDE 'MAPHD'
      INCLUDE 'MAPV4'

*  Status:
      INTEGER IFAIL              ! Global status

*  Local Variables:
      INTEGER STATUS             ! Starlink status

*.

*  Reset global status.
      IFAIL = 0

*  Begin a new Starlink error context.
      STATUS = SAI__OK
      CALL ERR_MARK

*  Release index NDF and position array.
*  Release file by annulling the only HDS locator.
      CALL NDF_ANNUL( IDXNDF, STATUS )
      CALL NDF_ANNUL( POSNDF, STATUS )


*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         IFAIL = 18
         CALL ERR_FLUSH( STATUS )
      END IF
      CALL ERR_RLSE

*  Return.
      END
