      SUBROUTINE SP0WR( COMM, PARAMS, TITLE, STATUS )
*+
*  Name:
*     SP0WR

*  Purpose:
*     Implements the DIPSO command SP0WR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SP0WR( COMM, PARAMS, TITLE, STATUS )

*  Description:
*     The SP0WR command writes the contents of the current arrays
*     to an NDF in IUE SPECTRUM format 0.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The name of the command which invoked this routine. This will
*        usually be "SP0WR".
*     PARAMS = CHARACTER * ( * ) (Given)
*        Any text supplied by the user on the command line following the
*        command name.
*     TITLE = CHARACTER * ( * ) (Given)
*        The title.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-AUG-1994 (DSB):
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

*  Global Variables:
      INCLUDE 'DECLARE_STKS'     ! DIPSO array sizes, etc.
      INCLUDE 'DECLARE_DATA'     ! DIPSO current arrays
*        NPOINT = INTEGER (Read)
*           The number of data elements in the current arrays.

*  Arguments Given:
      CHARACTER COMM*(*)
      CHARACTER PARAMS*(*)
      CHARACTER TITLE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        NDFNM*255,         ! Full name of NDF structure.
     :        ROOT*255           ! Root name of NDF structure.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the current arrays are empty, issue a warning and return.
      IF( NPOINT .EQ. 0 ) THEN
         CALL MSGOUT( COMM, 'Current arrays are empty - not written.',
     :                .TRUE., STATUS )
         GO TO 999
      END IF

*  Give a warning if two or more parameters were supplied.
      CALL EXPAR( COMM, PARAMS, 1, STATUS )

*  Get the name of the NDF.
      CALL GET0C( PARAMS, 1, .FALSE., COMM, 'Name of the NDF ' //
     :            'structure', ' ', ROOT, STATUS )

*  Tidy the NDF name.
      CALL NDFNAM( ROOT, ' ', NDFNM, STATUS )

*  Tell the user what is about to happen.
      CALL MSG_SETC( 'NDFNAM', NDFNM )
      CALL MSGOUT( COMM, 'Writing current arrays to NDF ''^NDFNAM'''//
     :             ' in SPECTRUM format 0.',
     :             .FALSE., STATUS )

*  Write the current array contents. This will put two zeros in the NDF
*  DATA array at each break point. Use WORV = 1.0.
      CALL WRNDF( COMM, NDFNM, 1.0, TITLE, STATUS )

*  Jump to here if an error occurs.
 999  CONTINUE

*  If an error has occurred, re-report it with less information if the
*  current MSG message filtering level is not verbose.
      CALL REREP( COMM, 'An error occurred while saving the '//
     :            'current arrays in SPECTRUM format 0.', STATUS )

      END
