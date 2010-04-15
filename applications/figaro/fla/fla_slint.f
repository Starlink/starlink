      SUBROUTINE FLA_SLINT( LBND, UBND, NSPEC, NLINES, PIND, ARRAY,
     :                      STATUS )
*+
*  Name:
*     FLA_SLINT

*  Purpose:
*     Interpolates across a series of emission lines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FLA_SLINT( LBND, UBND, NSPEC, NLINES, PIND, ARRAY, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of each spectrum.
*     UBND = INTEGER (Given)
*        The upper bound of each spectrum.
*     NSPEC = INTEGER (Given)
*        The number of spectra in the array to be sky subtracted.
*     NLINES = INTEGER (Given)
*        The number of emission lines to be interpolated across in
*        each spectrum.
*     PIND( 2, NLINES ) = INTEGER (Given)
*        The pixel indices of the bounds of the emission lines to be
*        interpolated across.
*     OUT( LBND:UBND, NSPEC ) = REAL (Given & Returned)
*        The one- or two-dimensional array spectra to be cleaned of
*        emission lines.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine is not tolerant of bad values.  Therefore, all the
*     supplied spectra should be checked that they contain no bad
*     values.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 October 15 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER LBND
      INTEGER UBND
      INTEGER NSPEC
      INTEGER NLINES
      INTEGER PIND( 2, NLINES )

*  Arguments Given and Returned:
      REAL ARRAY( LBND:UBND, NSPEC )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL HIGH                  ! Value immediately following a line
      INTEGER I                  ! Pixel counter
      INTEGER J                  ! Spectrum counter
      INTEGER L                  ! Line counter
      REAL LOW                   ! Value immediately prior to a line
      REAL SCALE                 ! Interpolation scale factor

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop for each spectrum.
      DO J = 1, NSPEC

*  Loop for each line.
         DO L = 1, NLINES

*  Find the values before and after the line.
            LOW = ARRAY( PIND( 1, L ) - 1, J )
            HIGH = ARRAY( PIND( 2, L ) + 1, J )

*  Find the scale factor.
            SCALE = ( HIGH - LOW ) / ( PIND( 2, L ) - PIND( 1, L ) + 2 )

*  Interpolate across the line.
            DO I = PIND( 1, L ), PIND( 2, L )
               ARRAY( I, J ) = REAL( I - PIND( 1, L ) + 1 ) * SCALE +
     :                         LOW
            END DO
         END DO
      END DO

      END
