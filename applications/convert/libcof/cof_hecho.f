      SUBROUTINE COF_HECHO( NLINES, TEXT, STATUS )
*+
*  Name:
*     COF_HECHO

*  Purpose:
*     Writes history text to the FITS headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_HECHO( NLINES, TEXT, STATUS )

*  Description:
*     This routine appends the history text associated with an
*     NDF to the current FITS header.  It is not called directly (by
*     COF_WHISR), but is passed as an external argument to routine
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
*     The NDF and the FITS file must already be open.  The current
*     HDU in the FITS file should be the primary and the other headers
*     should have been written.

*  Notes:
*     -  The argument list should not be changed.
*     -  There is no error checking of the FITSIO status.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 January 13 (MJC):
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
      INCLUDE 'COF_CMN'          ! Common block for passing required
                                 ! additional arguemnts
*        FFUNIT = INTEGER (Read)
*           The Fortran logical unit number of the FITS file.

*  Arguments Given:
      INTEGER NLINES
      CHARACTER * ( * ) TEXT( NLINES )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FSTAT              ! FITSIO status
      INTEGER I                  ! Loop counter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, NLINES
         CALL FTPHIS( FFUNIT, TEXT( I ), FSTAT )
      END DO

      END
