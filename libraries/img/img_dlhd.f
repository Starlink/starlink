      SUBROUTINE IMG_DLHD( PARAM, XNAME, ITEM, STATUS )
*+
* Name:
*    IMG_DLHD

*  Purpose:
*     Deletes a "header" item.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_DLHD( PARAM, XNAME, ITEM, STATUS )

*  Description:
*     This routine deletes a "header" item from the named NDF
*     extension. "Header" items include both FITS header records and
*     package specific NDF extension information. The values of FITS
*     header records are deleted by setting the XNAME argument to the
*     value 'FITS' and ITEM to the required keyword.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name of the NDF whose header item will be deleted.
*        (case insensitive).
*     XNAME = CHARACTER * ( * ) (Given)
*        Name of the NDF extension to which contains the header item
*        ('FITS' to delete a FITS header record).
*     ITEM = CHARACTER * ( * ) (Given)
*        Name of the NDF extension item to be deleted or the FITS header
*        record keyword.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - The item names for any extension may be hierarchical
*     (i.e. ING.DETHEAD get the FITS header value for "ING DETHEAD",
*     BOUNDS.MAXX gets the value of the MAXX component of the BOUNDS
*     structure in the named extension).

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     27-JUL-1994 (PDRAPER):
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
      INCLUDE 'IMG_ERR'          ! IMG_ error codes
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters

*  Global variables:
      INCLUDE 'IMG_ECB'          ! IMG Extension Control Block
*        ECB_XREAD( IMG__MXPAR, IMG__MXEXT ) = LOGICAL (Read)
*        The type of access allowed to the extension. .TRUE. for
*        readonly .FALSE. otherwise.
      
      INCLUDE 'IMG_PCB'          ! IMG Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*           NDF identifiers

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) XNAME
      CHARACTER * ( * ) ITEM
      
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
      
*  Initialise IMG to access the extension (if not already doing so).
         CALL IMG1_EXINI( SLOT, XNAME, .FALSE., ESLOT, STATUS )

*  Check that the extension may be written to (and hence deleted from).
         IF ( STATUS .EQ. SAI__OK ) THEN 
            IF ( ECB_XREAD( SLOT, ESLOT ) ) THEN

*  Extension is readonly. Complain.
               STATUS = IMG__NOACC
               CALL MSG_SETC( 'XNAME', XNAME )
               CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
               CALL ERR_REP( 'IMG_DLHD_NODEL', 'It is not possible ' //
     :              'to delete header items from the ^XNAME ' //
     :              'extension of the NDF ^NDF (possible programming '//
     :              'error).', STATUS )

*  Now branch according to the "type" of extension which we are dealing
*  with. FITS requires its own methods.
            ELSE 
               IF ( CHR_SIMLR( 'FITS', XNAME ) ) THEN

*  Need to delete the required item from the FITS character array.
                  CALL IMG1_DLFT( SLOT, ITEM, STATUS )
               ELSE

*  Need to delete the named item from a "normal" extension.
                 CALL IMG1_DLEX( SLOT, ESLOT, ITEM, STATUS )
               END IF
            END IF
         END IF
      END IF
      END
* $Id$
