      SUBROUTINE PON_SPOL( XMIN, XMAX, NDAT, WEIGHT,
     :                     NCOEFF, COEFF, EPS, STATUS )
*+
*  Name:
*     PON_SPOL

*  Purpose:
*     Display the results of a polynomial fit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_SPOL( XMIN, XMAX, NDAT, WEIGHT,
*    :                NCOEFF, EPS, STATUS )

*  Description:
*     The results of a least squares fit to a polynomial are
*     delivered to the user.

*  Arguments:
*     XMIN = REAL (Given)
*        Minimum X value.
*     XMAX = REAL (Given)
*        Maximum X value.
*     NDAT = INTEGER (Given)
*        Number of data.
*     WEIGHT = LOGICAL (Given)
*        Whether the fit was weighted.
*     NCOEFF = INTEGER (Given)
*        Number of polynomial coefficients.
*     COEFF( * ) = DOUBLE PRECISION (Given)
*        Coefficients of fit (size NCOEFF + 1)
*     EPS = DOUBLE PRECISION (Given)
*        Fit RMS value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     22-AUG-1996 (PDRAPER):
*        Original version, based upon PON_SHOPOLY.
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
      INTEGER NDAT
      LOGICAL WEIGHT
      INTEGER NCOEFF
      DOUBLE PRECISION EPS
      DOUBLE PRECISION COEFF( * )

*  Status:
      INTEGER STATUS

*  Local variables:
      INTEGER ICOEFF

*.

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Deliver a summary of the fit results to the user.
      CALL MSG_SETI( 'NPOLY', NCOEFF )
      CALL MSG_BLANK( STATUS )

      IF ( WEIGHT ) THEN
         CALL MSG_OUT( ' ', 'Weighted least squares fit to a ' //
     :                 'polynomial of order ^NPOLY.', STATUS )
      ELSE
         CALL MSG_OUT( ' ', 'Unweighted least squares fit to a ' //
     :                 'polynomial of order ^NPOLY.', STATUS )
      END IF

      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', 'Statistics of the data:', STATUS )
      CALL MSG_SETI( 'NDAT', NDAT )
      CALL MSG_OUT( ' ', '   ^NDAT data points.', STATUS )
      CALL MSG_SETR( 'XMIN', XMIN )
      CALL MSG_OUT( ' ', '   Minimum X value = ^XMIN', STATUS )
      CALL MSG_SETR( 'XMAX', XMAX )
      CALL MSG_OUT( ' ', '   Maximum X value = ^XMAX', STATUS )

*  The fit results.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', 'Fit results:', STATUS )
      CALL MSG_SETI( 'NPOLY', NCOEFF )
      CALL MSG_OUT( ' ', '   Polynomial order = ^NPOLY', STATUS )
      CALL MSG_SETD( 'SJ', EPS)
      CALL MSG_OUT( ' ', '   RMS residual of fit = ^SJ', STATUS )
      CALL MSG_OUT( ' ', '   Polynomial coefficients:', STATUS )

      DO 20 ICOEFF = 1, NCOEFF + 1
         CALL MSG_SETD( 'COEFF', COEFF( ICOEFF ) )
         CALL MSG_OUT( ' ', '      ^COEFF', STATUS )
 20   CONTINUE


      END
* $Id$
