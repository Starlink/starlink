      SUBROUTINE COI_HECHO( NLINES, TEXT, STATUS )
*+
*  Name:
*     COI_HECHO

*  Purpose:
*     Writes history text to the FITS headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_HECHO( NLINES, TEXT, STATUS )

*  Description:
*     This routine appends the history text associated with an
*     NDF to the current IRAF OIF header.  It is not called directly
*     (by COI_WHISR), but is passed as an external argument to routine
*     NDF_HOUT.  (It is an equivalent to NDF_HECHO.  See SUN/33 for
*     more details.)

*  Arguments:
*     NLINES = INTEGER (Given)
*        Number of lines of history text.
*     TEXT( NLINES ) = CHARACTER * ( * ) (Given)
*        The history text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The NDF and the IRAF file must already be open.  The other
*     headers should have been written.

*  Notes:
*     -  The argument list should not be changed.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 March 25 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'COI_CMN'          ! Common block for passing required
                                 ! additional arguments
*        FIMDES = INTEGER (Read)
*           The Fortran logical unit number of the IRAF file.

*  Arguments Given:
      INTEGER NLINES
      CHARACTER * ( * ) TEXT( NLINES )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) HEADER  ! Output header card
      INTEGER I                  ! Loop counter

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Append the lines of history text to the IRAF headers.
      DO I = 1, NLINES
         HEADER = 'HISTORY   '//TEXT( I )
         CALL ADLINE( FIMDES, HEADER )
      END DO

      END
