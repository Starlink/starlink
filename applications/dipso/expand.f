      SUBROUTINE DPEXPAND( COMM, PARAMS, STATUS )
*+
*  Name:
*     DPEXPAND

*  Purpose:
*     Implements the DIPSO command EXPAND.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DPEXPAND( COMM, PARAMS, STATUS )

*  Description:
*     The EXPAND command stores expansion factors to apply to various
*     parts of the next plot.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The name of the command which invoked this routine. This will
*        usually be "EXPAND", but could conceivably be something else.
*     PARAMS = CHARACTER * ( * ) (Given)
*        Any text supplied by the user on the command line following the
*        command name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1994 (DSB):
*        Original version.
*     08-MAR-2007 (TIMJ):
*        Rename to DPEXPAND to avoid NCAR namespace clash
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'DECLARE_PLTS'     ! Common blocks holding info about the
                                 ! current plots
*        HTFAC( NHTFAC ) = REAL (Write)
*           Expansion factors for various parts of subsequent plot.
*        MJTICK = INTEGER( Read)
*           Index of major tick marks expansion factor.
*        MNTICK = INTEGER( Read)
*           Index of minor tick marks expansion factor.
*        NUMLAB = INTEGER( Read)
*           Index of numeric labels expansion factor.
*        TXTLAB = INTEGER( Read)
*           Index of text labels expansion factor.
*        MARKS = INTEGER( Read)
*           Index of marker expansion factor.
*        PWRITE = INTEGER( Read)
*           Index of PWRITE text expansion factor.

*  Arguments Given:
      CHARACTER COMM*(*)
      CHARACTER PARAMS*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a string

*  Local Variables:
      CHARACTER
     :        C*1,               ! Current character
     :        PARTS*80           ! String containing parts identifiers

      INTEGER
     :        I                  ! Loop count

      REAL
     :        FACTOR             ! Expansion factor.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Give a warning if three or more parameters were supplied.
      CALL EXPAR( COMM, PARAMS, 2, STATUS )

*  Get the expansion factor to use.
      CALL GET0R( PARAMS, 1, .false., COMM, 'Expansion factor', 1.0,
     :            FACTOR, STATUS )

*  Get the string which specifies which components of the plot the
*  expansion factor is to be applied to.
      CALL GET0C( PARAMS, 2, .TRUE., COMM, 'Parts to be expanded', ' ',
     :            PARTS, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If the string is blank, scale all parts of the plot with the given
*  factor.
      IF( PARTS .EQ. ' ' ) THEN
         DO I = 1, NHTFAC
            HTFAC( I ) = FACTOR
         END DO

*  Otherwise, only scale the parts specified.
      ELSE

*  Clean up the string.
         CALL CHR_RMBLK( PARTS )
         CALL CHR_CLEAN( PARTS )
         CALL CHR_UCASE( PARTS )

*  Loop round each character in the supplied string.
         DO I = 1, CHR_LEN( PARTS )
            C = PARTS( I : I )

*  If this character is an A, scale the major tick marks.
            IF( C .EQ. 'A' ) THEN
               HTFAC( MJTICK ) = FACTOR

*  If this character is an I, scale the minor tick marks.
            ELSE IF( C .EQ. 'I' ) THEN
               HTFAC( MNTICK ) = FACTOR

*  If this character is an N, scale the numerical axis labels.
            ELSE IF( C .EQ. 'N' ) THEN
               HTFAC( NUMLAB ) = FACTOR

*  If this character is a T, scale the textual axis labels.
            ELSE IF( C .EQ. 'T' ) THEN
               HTFAC( TXTLAB ) = FACTOR

*  If this character is an M, scale the markers.
            ELSE IF( C .EQ. 'M' ) THEN
               HTFAC( MARKS ) = FACTOR

*  If this character is a P, scale the text created by the PWRITE
*  command.
            ELSE IF( C .EQ. 'P' ) THEN
               HTFAC( PWRITE ) = FACTOR

*  Otherwise, issue a warning.
            ELSE
               CALL MSG_SETC( 'C', C )
               CALL MSGOUT( COMM, 'Ignoring illegal character ''^C''.',
     :                      .TRUE., STATUS )
            END IF

         END DO

      END IF

*  Jump to here if an error occurs.
 999  CONTINUE

*  If an error has occurred, re-report it with less information if the
*  current MSG message filtering level is not verbose.
      CALL REREP( COMM, 'An error occurred while storing new '//
     :            'graphics expansion factors.', STATUS )

      END
