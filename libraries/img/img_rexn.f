      SUBROUTINE IMG_REXN( PARAM, N, EXTEN, STATUS )
*+
* Name:
*    IMG_REXN

*  Purpose:
*    Returns the name of the Nth extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IMG_NEX( PARAM, N, EXTEN, STATUS )

*  Description:
*     This routine returns the name of the Nth extension in an NDF. By
*     incrementing index N all the extension names in an NDF may be
*     queried.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name (case insensitive).
*     N = INTEGER (Given)
*        The index of the extension.
*     EXTEN = CHARACTER * ( * ) (Returned)
*        The name of the extension. Blank when no extension with the
*        given index exists.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     15-AUG-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IMG_CONST'        ! IMG_ parameters

*  Global variables:
      INCLUDE 'IMG_PCB'          ! IMG Parameter Control Block
*        PCB_INDF( IMG__MXPAR ) = INTEGER (Read)
*           NDF identifiers

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      INTEGER N

*  Arguments Returned:
      CHARACTER * ( * ) EXTEN

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Strings are the same apart from case
      EXTERNAL CHR_NTH
      CHARACTER * ( 2 ) CHR_NTH  ! Nth character ordinal

*  Local Variables:
      CHARACTER * ( IMG__SZPAR ) VPAR ! Validated parameter name
      CHARACTER * ( 2 ) TH       ! Nth character ordinal
      INTEGER SLOT               ! Parameter slot number
      LOGICAL WASNEW             ! True if parameter is new
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the parameter name and obtain its slot number.
      CALL IMG1_VPAR( PARAM, VPAR, STATUS )
      CALL IMG1_GTSLT( VPAR, .FALSE., SLOT, WASNEW, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Get the Nth extension name from NDF.
         CALL NDF_XNAME( PCB_INDF( SLOT ), N, EXTEN, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Failed in attempt to access the extension, add information to any
*  error messages.
            TH = CHR_NTH( N )
            CALL NDF_MSG( 'NDF', PCB_INDF( SLOT ) )
            CALL MSG_SETI( 'N', N )
            CALL MSG_SETC( 'TH', TH )
            CALL ERR_REP( 'IMG_NEX_ERR', 'Failed to access the ' //
     :           '^N^TH extension in the NDF ^NDF.', STATUS )
         END IF
      END IF
      END
* $Id$
