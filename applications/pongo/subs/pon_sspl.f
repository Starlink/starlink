      SUBROUTINE PON_SSPL( XMIN, XMAX, NDAT, WEIGHT, NPOLY, EPS,
     :                     IKNOTS, SFD, SFACT, STATUS )
*+
*  Name:
*     PON_SSPL

*  Purpose:
*     Display the results of a spline fit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SUBROUTINE PON_SSPL( XMIN, XMAX, NDATFIT,
*                          WEIGHT, NPOLY, EPSR, IKNOTS, SFD,
*                          SFACT, STATUS )

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
*     NPOLY = INTEGER (Given)
*        Number of polynomial coefficients.
*     EPSR = REAL (Given)
*        Fit RMS value.
*     IKNOTS = INTEGER (Given)
*        Number of knots used in fit.
*     SFD = INTEGER (Given)
*        FIO file descriptor for spline file.
*     SFACT = REAL (Given)
*        The "smoothing" factor of the spline fit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     30-AUG-1996 (PDRAPER):
*        Original version, based upon PON_SPOL.
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
      INTEGER NPOLY
      REAL EPS
      INTEGER IKNOTS
      INTEGER SFD
      REAL SFACT

*  Status:
      INTEGER STATUS

*  Local variables:
      CHARACTER * ( 132 ) FNAME

*.

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Deliver a summary of the fit results to the user.
      CALL MSG_SETI( 'NPOLY', NPOLY )
      CALL MSG_BLANK( STATUS )

      IF ( WEIGHT ) THEN
         CALL MSG_OUT( ' ', 'Weighted spline fit', STATUS )
      ELSE
         CALL MSG_OUT( ' ', 'Unweighted spline fit', STATUS )
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
      CALL MSG_SETI( 'NPOLY', NPOLY )
      CALL MSG_OUT( ' ', '   Spline order = ^NPOLY', STATUS )
      CALL MSG_SETI( 'IKNOTS', IKNOTS )
      CALL MSG_OUT( ' ', '   Number of knots = ^IKNOTS', STATUS )
      CALL MSG_SETR( 'SJ', EPS)
      CALL MSG_OUT( ' ', '   RMS residual of fit = ^SJ', STATUS )
      CALL MSG_SETR( 'SM', SFACT)
      CALL MSG_OUT( ' ', '   Smoothing factor = ^SM', STATUS )

*  The spline file name.
      CALL FIO_FNAME( SFD, FNAME, STATUS )
      CALL MSG_SETC( 'FNAME', FNAME )
      CALL MSG_OUT( ' ', '   Spline data stored in file ^FNAME',
     :              STATUS )

      END
* $Id$
