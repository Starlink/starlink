      SUBROUTINE RDCAT( PARAMS, WORV, NAME, STATUS )
*+
*  Name:
*     RDCAT

*  Purpose:
*     Implements the DIPSO command RDCAT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDCAT( PARAMS, WORV, NAME, STATUS )

*  Description:
*     The RDCAT command reads the contents of catalogue into the current
*     arrays.

*  Arguments:
*     PARAMS = CHARACTER * ( * ) (Given)
*        Any text supplied by the user on the command line following the
*        command name.
*     WORV = REAL (Returned)
*        The "wavelength or velocity" value for the catalogue.
*     NAME = CHARACTER * ( * ) (Returned)
*        The catalogue NAME, or blank if it has no name.
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
*     10-JUL-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'CAT_PAR'          ! CAT__ constants

*  Arguments Given:
      CHARACTER PARAMS*(*)

*  Arguments Returned:
      REAL WORV
      CHARACTER NAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      CHARACTER COMM*5           ! DIPSO command name
      PARAMETER ( COMM = 'RDCAT' )

*  Local Variables:
      CHARACTER FILE*255         ! Catalogue file name
      CHARACTER OLDNAM*(CAT__SZCMP) ! Previous column name
      CHARACTER XNAME*(CAT__SZCMP)  ! X column name
      CHARACTER YNAME*(CAT__SZCMP)  ! Y column name
      INTEGER CI                 ! Catalogue id
      INTEGER GX                 ! Catalogue X column id
      INTEGER GY                 ! Catalogue Y column id

*  Set initial column names, and ensure they are retained between
*  invocations of this command.
      DATA XNAME /'WAVELENGTH'/, YNAME /'FLUX'/
      SAVE XNAME, YNAME
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Give a warning if too many parameters were supplied.
      CALL EXPAR( COMM, PARAMS, 1, STATUS )

*  Get a CAT identifier for the catalogue.
      CALL GETCAT( PARAMS, 1, .FALSE., COMM, 'The catalogue to be read',
     :             ' ', 'READ', CI, FILE, STATUS )

*  Get the name of the column containing the X data.
      OLDNAM = XNAME
      CALL GTCTCL( PARAMS, 2, .FALSE., COMM, 'The name of the X '//
     :             'column', OLDNAM, CI, GX, XNAME, STATUS )

*  Get the name of the column containing the Y data.
      OLDNAM = YNAME
      CALL GTCTCL( PARAMS, 3, .FALSE., COMM, 'The name of the Y '//
     :             'column', OLDNAM, CI, GY, YNAME, STATUS )

*  Tell the user what is about to happen.
      CALL MSG_SETC( 'FILE', FILE )
      CALL MSGOUT( COMM, 'Reading data from catalogue ''^FILE'' into '//
     :             'the current arrays.', .FALSE., STATUS )

*  Read the data from the catalogue into the current arrays.
      CALL RDCTD( COMM, CI, GX, GY, WORV, NAME, STATUS )

*  Close the catalogue.
      CALL CAT_TRLSE( CI, STATUS )

*  Re-report any errors that may have occurred, giving less information
*  if the current message filtering level is not set to 3 (verbose).
      CALL REREP( COMM, 'An error occurred while reading data '//
     :            'into the current arrays.', STATUS )

      END
