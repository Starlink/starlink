      SUBROUTINE IMG_WRHD<T>( PARAM, XNAME, ITEM, COMMEN, VALUE,
     :                        STATUS )
*+
*  Name:
*    IMG_WRHD<T>

*  Purpose:
*     Writes a "header" item.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_WRHDX( PARAM, XNAME, ITEM, COMMEN, VALUE, STATUS )

*  Description:
*     This routine writes the value of a "header" item into an NDF.
*     "Header" items include both FITS header records and package
*     specific NDF extension information. The values of FITS header
*     records are written by setting the XNAME argument to the value
*     'FITS' and ITEM to the required keyword.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name of the NDF whose header items are to be written
*        (case insensitive).
*     XNAME = CHARACTER * ( * ) (Given)
*        Name of the NDF extension to be written to ('FITS' to write a
*        FITS header record).
*     COMMEN = CHARACTER * ( * ) (Given)
*        If XNAME is 'FITS' then this is used as a comment to enter
*        with the record.
*     VALUE = <COMM> (Given)
*        The value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - There is a version of this routine for writing header items
*     of various types. Replace the "x" in the routine name by C, L, D,
*     R, or I as appropriate.
*
*     - The item names for any extension may be hierarchical
*     (i.e. ING.DETHEAD writes the FITS header value for "ING DETHEAD",
*     BOUNDS.MAXX the value of the MAXX component of the BOUNDS
*     structure in the named extension). Writing hierarchical records in
*     FITS records is strongly discouraged.
*
*     - If the extension name isn't 'FITS' then the COMMEN string is
*     used as a "type" for any structures created.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     22-JUL-1994 (PDRAPER):
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

*  Global Variables:
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
      CHARACTER * ( * ) COMMEN
      <TYPE> VALUE

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

*  Initialise IMG to write the extension (if not already doing so).
         CALL IMG1_EXINI( SLOT, XNAME, .TRUE., ESLOT, STATUS )

*  Check that the extension may be written to.
         IF ( ECB_XREAD( SLOT, ESLOT ) ) THEN

*  Extension is readonly. Complain.
            STATUS = IMG__NOACC
            CALL MSG_SETC( 'XNAME', XNAME )
            CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
            CALL ERR_REP( 'IMG_WRHDX_NOWRT', 'The ^XNAME extension ' //
     :           'of the NDF ^NDF cannot be written to (possible ' //
     :           'programming error).', STATUS )
         ELSE

*  Now branch according to the "type" of extension which we are dealing
*  with. FITS and "normal" extensions require their own methods.
            IF ( CHR_SIMLR( 'FITS', XNAME ) ) THEN

*  Need to write the item into the FITS character array.
               CALL IMG1_WRFT<T>( SLOT, ITEM, COMMEN, VALUE, STATUS )
            ELSE

*  Need to write the named item into a "normal" (primitive) object, this
*  may be hierarchical.
               CALL IMG1_WREX<T>( SLOT, ESLOT, ITEM, COMMEN, VALUE,
     :                            STATUS )
            END IF
         END IF
      END IF
      END
* $Id$
