      SUBROUTINE GDSET( STATUS )
*+
*  Name:
*     GDSET

*  Purpose:
*     Selects a current graphics device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GDSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Usage:
*     gdset device

*  Description:
*     This application selects a current graphics device.  This device
*     will be used for all applications requiring an image-display
*     until changed explicitly.

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The graphics device to become the current graphics device.

*  Examples:
*     gdset xwindows
*        Makes the xwindows device the current graphics device.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 June 27 (MJC):
*        Original version.
*     1991 October 17 (MJC):
*        Added an AGI_ANNUL to ensure that the global parameter is
*        updated every invocation.
*     5-DEC-2001 (DSB):
*        Report output file when using PS devices.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'       ! Global SSE definitions

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER STRING*80     ! PGPLOT information
      INTEGER LENSTR          ! Used length of STRING
      INTEGER PICID           ! AGI input picture identifier
*.

*  Check the inherited status on entry.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Open the AGI workstation to read the device.
      CALL AGI_ASSOC( 'DEVICE', 'READ', PICID, STATUS )

*  If a postscript device has been opedn, tell the user the name of the
*  output file, and warn them that this file will be over-writen by each
*  subsequent graphics command.
      CALL PGQINF( 'STATE', STRING, LENSTR )
      IF( STRING .EQ. 'OPEN' ) THEN

         CALL PGQINF( 'TYPE', STRING, LENSTR )
         IF( INDEX( STRING, 'PS' ) .GT. 0 ) THEN

            CALL PGQINF( 'HARDCOPY', STRING, LENSTR )
            IF( STRING .EQ. 'YES' ) THEN

               CALL PGQINF( 'FILE', STRING, LENSTR )
               IF( STRING .NE. ' ' ) THEN 
                  CALL MSG_BLANK( STATUS )
                  CALL MSG_SETC( 'F', STRING )
                  CALL MSG_OUT( ' ', 'All output will go to file ^F '//
     :                          'by default. '//
     :                          'Each subsequent graphics command '//
     :                          'will overwrite any existing file '//
     :                          'with this name.', STATUS )
                  CALL MSG_BLANK( STATUS )
               END IF

            END IF

         END IF

      END IF

*  Annul the workstation.
      CALL AGI_ANNUL( PICID, STATUS )

*  Issue a context message if an error has occurred.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GDSET_ERR', 'GDSET: Error selecting a '//
     :                 'graphics device.', STATUS )
      END IF

      END
