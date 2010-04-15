      SUBROUTINE FLA_OISS( LBND, UBND, NSPEC, IN, PIND, SKY, OUT,
     :                     STATUS )
*+
*  Name:
*     FLA_OISS

*  Purpose:
*     Subtracts a sky spectrum from spectra using the strength of a
*     prominent emission line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FLA_OISS( LBND, UBND, NSPEC, IN, PIND, SKY, OUT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of each spectrum.
*     UBND = INTEGER (Given)
*        The upper bound of each spectrum.
*     NSPEC = INTEGER (Given)
*        The number of spectra in the array to be sky subtracted.
*     IN( LBND:UBND, NSPEC ) = REAL (Given)
*        The one- or two-dimensional array spectra to sky subtract.
*     PIND( 3 ) = INTEGER (Given)
*        The pixel indices of the bounds (first and third elements) of
*        the emission line used to normalise the sky spectrum.  The
*        second value is the pixel containing the nominal wavelength of
*        the emission line.
*     SKY( LBND:UBND ) = REAL (Given)
*        The sky spectrum to be subtracted from the object spectra
*        after normalisation with the emission line's strength.
*     OUT( LBND:UBND, NSPEC ) = REAL (Returned)
*        The one- or two-dimensional array spectra after sky
*        subtraction.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     As this routine merely sums the line profile rather than
*     performing a fit, it is not tolerant of bad values.  Therefore,
*     all the supplied spectra should be checked that they contain no
*     bad values.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 October 14 (MJC):
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
      INTEGER PIND( 3 )
      REAL IN( LBND:UBND, NSPEC )
      REAL SKY( LBND:UBND )

*  Arguments Returned:
      REAL OUT( LBND:UBND, NSPEC )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CLL                ! Continuum lower limit below the line
      INTEGER CLU                ! Continuum upper limit below the line
      INTEGER CUL                ! Continuum lower limit above the line
      INTEGER CUU                ! Continuum upper limit above the line
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      INTEGER N                  ! Pixel counter
      REAL OBJCON                ! Average neighbouring object continuum
      REAL NORM                  ! Normalisation factor
      REAL SKYLIN                ! Strength of the sky emission line
      REAL SKYCON                ! Average neighbouring sky continuum
      REAL SUM                   ! Summation of continuum or line

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the strength of the emission line in the sky spectrum.
*  ===========================================================

*  Get the limits for the continuum.  Use the four pixels either side
*  of the line.  Should actually be more careful than this but will do
*  for the moment.
      CLL = MAX( LBND, PIND( 1 ) - 4 )
      CLU = MAX( LBND, PIND( 1 ) - 1 )
      CUL = MIN( UBND, PIND( 3 ) + 1 )
      CUU = MIN( UBND, PIND( 3 ) + 4 )

*  Initialise summation variables.
      SUM = 0.0
      N = 0

*  Sum the sky continuum values.
      DO I = CLL, CLU
         SUM = SUM + SKY( I )
         N = N + 1
      END DO
      DO I = CUL, CUU
         SUM = SUM + SKY( I )
         N = N + 1
      END DO

*  Calculate the average continuum.
      SKYCON = SUM / REAL( N )

*  Initialise summation variables.
      SUM = 0.0
      N = 0

*  Sum the values within the emission line.
      DO I = PIND( 1 ), PIND( 2 )
         SUM = SUM + SKY( I )
         N = N + 1
      END DO

*  Find the line strength of emission line.
      SKYLIN = SUM - REAL( N ) * SKYCON

*  Loop for each object spectrum.
      DO J = 1, NSPEC

*  Find the strength of the emission line in the object spectrum.
*  ==============================================================

*  Initialise summation variables.
         SUM = 0.0
         N = 0

*  Sum the sky continuum values.
         DO I = CLL, CLU
            SUM = SUM + IN( I, J )
            N = N + 1
         END DO
         DO I = CUL, CUU
            SUM = SUM + IN( I, J )
            N = N + 1
         END DO

*  Calculate the average continuum.
         OBJCON = SUM / REAL( N )

*  Initialise summation variables.
         SUM = 0.0
         N = 0

*  Sum the values within the emission line.
         DO I = PIND( 1 ), PIND( 2 )
            SUM = SUM + IN( I, J )
            N = N + 1
         END DO

*  Derive the normalisation factor to be applied to scale the sky
*  spectrum to match the object spectrum, being the ratio of the line
*  strengths in the object to the sky.
         NORM = ( SUM - REAL( N ) * OBJCON ) / SKYLIN

*  Subtract the scaled sky spectrum from the object spectrum.
         DO I = LBND, UBND
            OUT( I, J ) = IN( I, J ) - NORM * SKY( I )
         END DO
      END DO

      END
