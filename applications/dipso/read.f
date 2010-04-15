      SUBROUTINE READ( COMM, PARAMS, WORV, TITLE, STATUS )
*+
*  Name:
*     READ

*  Purpose:
*     Implements the DIPSO command READ.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL READ( COMM, PARAMS, WORV, TITLE, STATUS )

*  Description:
*     The READ command reads the contents of an NDF into the current
*     arrays.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The name of the command which invoked this routine. This will
*        usually be "READ".
*     PARAMS = CHARACTER * ( * ) (Given)
*        Any text supplied by the user on the command line following the
*        command name.
*     WORV = REAL (Returned)
*        The "wavelength or velocity" value for the NDF.
*     TITLE = CHARACTER * ( * ) (Returned)
*        The NDF title, or blank if the NDF has no title.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the X units are km/s, WORV is the rest wavelength (in
*     Angstroms) to which the velocities are referenced, divided by
*     he speed of light (in km/s). If the X units are not km/s, then
*     WORV will be set to 1.0.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-AUG-1994 (DSB):
*        Original version.
*     13-DEC-1995 (DSB):
*        Remove LOC argument from GETNDF call. This argument returned a
*        primary locator to keep the file open, but the NDF library now
*        looks after it all.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Arguments Given:
      CHARACTER COMM*(*)
      CHARACTER PARAMS*(*)

*  Arguments Returned:
      REAL WORV
      CHARACTER TITLE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        USYS*30,           ! Default X-axis system
     :        UUNIT*30           ! Default X-axis unit

      INTEGER
     :        INDF               ! NDF identifier

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Give a warning if too many parameters were supplied.
      CALL EXPAR( COMM, PARAMS, 3, STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get an identifier for the NDF.
      CALL GETNDF( PARAMS, 1, .FALSE., COMM, 'The NDF to be read',
     :             ' ', 'READ', ' ', INDF, STATUS )

*  Get the default X-axis System.
      CALL GET0C( PARAMS, 2, .TRUE., COMM, ' ', ' ', USYS, STATUS )

*  Get the default X-axis Unit.
      CALL GET0C( PARAMS, 3, .TRUE., COMM, ' ', ' ', UUNIT, STATUS )

*  Tell the user what is about to happen.
      CALL NDF_MSG( 'NDFNAM', INDF )
      CALL MSGOUT( COMM, 'Reading data from NDF ''^NDFNAM'' into the '//
     :             'current arrays.', .FALSE., STATUS )

*  Read the data from the NDF into the current arrays.
      CALL RDNDF( COMM, INDF, USYS, UUNIT, WORV, TITLE, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Re-report any errors that may have occurred, giving less information
*  if the current message filtering level is not set to 3 (verbose).
      CALL REREP( COMM, 'An error occurred while reading data '//
     :            'into the current arrays.', STATUS )

      END
