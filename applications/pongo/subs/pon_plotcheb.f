      SUBROUTINE PON_PLOTCHEB( XMIN, XMAX, NPOLY, COEFF, STATUS )
*+
*  Name:
*     PON_PLOTCHEB

*  Purpose:
*     Plot a polynomial curve expressed as a Chebyshev series.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_PLOTCHEB( XMIN, XMAX, NPOLY, COEFF, STATUS )

*  Description:
*     Use the routine PON_EVALCHEB to evaluate the polynomial and plot
*     the curve using calls to PGDRAW.

*  Arguments:
*     XMIN = REAL (Given)
*        Minimum X value.
*     XMAX = REAL (Given)
*        Minimum Y value.
*     NPOLY = INTEGER (Given)
*        Number of data.
*     COEFF( NPOLY ) = DOUBLE PRECISION (Given)
*        Polynomial coefficients.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: Peter W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     7-DEC-1992 (PCTR):
*        Original version (based upon PON_PLOTPOLY).
*     28-AUG-1996 (PDRAPER):
*        Converted to use non-NAG polynomial evaluation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Arguments Given:
      REAL XMIN
      REAL XMAX
      INTEGER NPOLY
      DOUBLE PRECISION COEFF( NPOLY )

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER NSTEP             ! Number of steps in plot
      PARAMETER ( NSTEP = 500 )

*  External references:
      DOUBLE PRECISION PON_EVALCHEB
      EXTERNAL PON_EVALCHEB

*  Local Variables:
      INTEGER I                 ! Loop index
      INTEGER IFAIL             ! NAG returned status flag

      DOUBLE PRECISION PVAL     ! Polynomial value
      DOUBLE PRECISION X        ! X value
      DOUBLE PRECISION XDIV1    ! Dividend value
      DOUBLE PRECISION XDIV2    ! Dividend value
      DOUBLE PRECISION XDENO    ! Denominator in X transformation
      DOUBLE PRECISION XNORM    ! Transformation to -1 <X< 1
      DOUBLE PRECISION XSTEP    ! X increment

*.

*  Evaluate intermediate constants.
      XDENO = DBLE( XMAX ) - DBLE( XMIN )
      XSTEP = XDENO / DBLE( NSTEP )

*  Evaluate first value.
      PVAL = PON_EVALCHEB( -1.0D0, COEFF, NPOLY, STATUS )

*  Check the returned status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*     Move the plotting pen to the beginning of the polynomial line.
         CALL PGMOVE( XMIN, REAL( PVAL ) )

*     Loop to plot the polynomial function.
         DO 10 I = 2, NSTEP + 1
            X = DBLE( XMIN ) + XSTEP * DBLE( I - 1 )
            XDIV1 = ( X - DBLE( XMIN ) )
            XDIV2 = ( DBLE( XMAX ) - X )
            XNORM = ( XDIV1 - XDIV2 ) / XDENO
            PVAL = PON_EVALCHEB( XNORM, COEFF, NPOLY, STATUS )

*     Check the returned status, if it is set, abort the
*     loop: if it is not, plot the line.
            IF ( STATUS .NE. SAI__OK ) THEN
               GO TO 20
            ELSE
               CALL PGDRAW( REAL( X ), REAL( PVAL ) )
            END IF
 10      CONTINUE
 20      CONTINUE
      END IF

*     Check the final status and report an error, if necessary.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PON_PLOTCHEB',
     :        'Chebyshev evaluation failed', STATUS )
      END IF

      END
* $Id$
