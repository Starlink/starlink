      SUBROUTINE IMG_EXHD( PARAM, XNAME, ITEM, FOUND, STATUS )
*+
*  Name:
*    IMG_EXHD

*  Purpose:
*     Checks for the existence of a "header" item.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_EXHD( PARAM, XNAME, ITEM, FOUND, STATUS )

*  Description:
*     This routine checks the existence of a "header" item. "Header"
*     items include both FITS header records and package specific NDF
*     extension information. The existence of FITS header records are
*     checked by setting the XNAME argument to the value 'FITS' and
*     ITEM to the required keyword.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name (case insensitive).
*     XNAME = CHARACTER * ( * ) (Given)
*        Name of the NDF extension ('FITS' to read a FITS header
*        record).
*     ITEM = CHARACTER * ( * ) (Given)
*        Name of the NDF extension item or FITS keyword.
*     FOUND = LOGICAL (Returned)
*        True if the header item is found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*     - The item names for any extension may be hierarchical
*     (i.e. ING.DETHEAD get the FITS header value for "ING DETHEAD",
*     BOUNDS.MAXX gets the value of the MAXX component of the BOUNDS
*     structure in the named extension).

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     11-AUG-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) XNAME
      CHARACTER * ( * ) ITEM

*  Arguments Returned:
      LOGICAL FOUND

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Strings are the same apart from case

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      LOGICAL WASNEW             ! Dummy
      INTEGER ESLOT              ! Extension slot number
      INTEGER SLOT               ! Parameter slot number

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the parameter name and obtain its slot number.
      CALL IMG1_VPAR( PARAM, VPAR, STATUS )
      CALL IMG1_GTSLT( VPAR, .FALSE., SLOT, WASNEW, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise IMG to read the extension (if not already doing so).
         CALL IMG1_EXINI( SLOT, XNAME, .FALSE., ESLOT, STATUS )

*  Now branch according to the "type" of extension which we are dealing
*  with. FITS requires its own methods.
         IF ( CHR_SIMLR( 'FITS', XNAME ) ) THEN

*  Need to extract the required item from the FITS character array.
            CALL IMG1_EXFT( SLOT, ITEM, FOUND, STATUS )
         ELSE

*  Need to locate the named item (which may be hierarchical).
            CALL IMG1_EXEX( SLOT, ESLOT, ITEM, FOUND, STATUS )
         END IF
      END IF
      END
* $Id$
