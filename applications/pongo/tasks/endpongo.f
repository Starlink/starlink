      SUBROUTINE ENDPONGO( STATUS )
*+
*  Name:
*     ENDPONGO

*  Purpose:
*     Close down the current plotting device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Close down the current plotting device, storing the current
*     picture description in the AGI database. The position and world
*     coordinate limits of the plot will be stored in the AGI database,
*     along with any comment text.
*
*     ENDPONGO must be executed before using a plotting application from
*     another package (e.g. KAPPA) -- failure to do so will result in
*     an AGI error and may corrupt the AGI database.

*  Usage:
*     endplot [comment] [device]

*  ADAM Parameters:
*     COMMENT = _CHAR (Read and Write)
*        A comment for the AGI database entry for the plot that has
*        just been completed. Any comment will be prefixed with the
*        string "PONGO:".
*        ["Final viewport"]
*     DEVICE = DEVICE (Read)
*        The name of the current plotting device (as set by BEGPLOT).
*        This parameter should never be prompted for.
*
*        The value of the global parameter GRAPHICS_DEVICE. If
*        GRAPHICS_DEVICE is not defined, the current value is used. If
*        the current value is not defined, the value will be prompted
*        for.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     4-JUL-1990 (PAH):
*        Original version.
*     28-NOV-1990 (JBVAD::PAH):
*        Made changes to take advantage of new AGI
*     4-DEC-1991 (PCTR):
*        Tidy up AGI calls.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     2-JUN-1994 (PDRAPER):
*        Removed commented out code.
*     20-JUN-1994 (PDRAPER):
*        Added check that device is open.
*     20-JUN-1994 (PDRAPER):
*        Sorted out close down AGI calls.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AGI_ERR'          ! AGI_ error codes

*  Status:
      INTEGER STATUS             ! Global status

*  External references
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used length of string
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open


*  Local Variables:
      CHARACTER * ( 80 ) AGICOM  ! Comment for the AGI database entry
      CHARACTER * ( 80 ) COMMNT  ! Comment for the AGI database entry

      INTEGER BASPIC             ! Base picture ID
      INTEGER PICIDD             ! Data picture ID

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the device is open.
      IF ( PON_DEVOP( .TRUE., STATUS ) ) THEN

*  Get the picture comment for AGI.
         CALL PAR_GET0C( 'COMMENT', COMMNT, STATUS )
         AGICOM = 'PONGO: ' //
     :        COMMNT( : MAX( 1, MIN( 80, CHR_LEN( COMMNT ) ) ) )

*  Save a data picture in the AGI database.
         CALL AGI_IBASE( BASPIC, STATUS )
         CALL AGI_SELP( BASPIC, STATUS )
         CALL AGP_SVIEW( 'DATA', AGICOM, PICIDD, STATUS )

*  End the AGI scope (annuls the outstanding identifiers and makes the
*  initial picture current).
         CALL AGI_END( -1, STATUS)

*  Deactivate the PGPLOT interface to AGI and close down AGI.
         CALL AGP_DEACT( STATUS )
         CALL AGI_CANCL( 'DEVICE', STATUS )

*  Check the returned status and report a contextual error message if
*  necessary.
      END IF
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'ENDPONGO_END',
     :                              'ENDPONGO: Error occurred while ' //
     :                              'closing the plotting device.',
     :                              STATUS )

      END
* $Id$
