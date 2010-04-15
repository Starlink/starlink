      SUBROUTINE PON_SHOLINE( XMIN, XMAX, YMIN, YMAX, NDAT, WEIGHT,
     :                        XMEAN, YMEAN, XSIG, YSIG, INTCPT, SLOPE,
     :                        INTCPTSIG, SLOPESIG, PCORR, STATUS )
*+
*  Name:
*     PON_SHOLINE

*  Purpose:
*     Display the results of a straight line fit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_SHOLINE( XMIN, XMAX, YMIN, YMAX, NDAT, WEIGHT,
*    :                  XMEAN, YMEAN, XSIG, YSIG, INTCPT, SLOPE,
*    :                  INTCPTSIG, SLOPESIG, PCORR, STATUS )

*  Description:
*     The results of a least squares fit to a straight line are
*     delivered to the user. The information is passed through the
*     argument list. MSG is used to build and deliver the messages.

*  Arguments:
*     XMIN = REAL (Given)
*        X-axis minimum.
*     XMAX = REAL (Given)
*        X-axis maximum.
*     YMIN = REAL (Given)
*        Y-axis minimum.
*     YMAX = REAL (Given)
*        Y-axis maximum.
*     NDAT = REAL (Given)
*        Number of data.
*     WEIGHT = LOGICAL (Given)
*        Whether the fit was weighted.
*     XMEAN = REAL (Given)
*        Mean X-axis value.
*     YMEAN = REAL (Given)
*        Mean Y-axis value.
*     XSIG = REAL (Given)
*        Standard deviation of X-axis data.
*     YSIG = REAL (Given)
*        Standard deviation of Y-axis data.
*     INTCPT = REAL (Given)
*        Y-axis intercept resulting from the fit.
*     SLOPE = REAL (Given)
*        Gradient resulting from the fit.
*     INTCPTSIG = REAL (Given)
*        Estimated standard deviation of the Y-axis intercept.
*     SLOPESIG = REAL (Given)
*        Estimated standard deviation of the gradient.
*     PCORR = REAL (Given)
*        Pearson correlation coefficient.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     10-APR-1990 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     7-DEC-1992 (PCTR):
*        New version of PON_SHOSTAT called PON_SHOLINE.
*     6-JUN-1994 (PDRAPER):
*        Removed unused arguments CHI2, CORRPROB and FITPROB.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL XMIN
      REAL XMAX
      REAL YMIN
      REAL YMAX

      INTEGER NDAT

      LOGICAL WEIGHT

      REAL XSIG
      REAL YSIG
      REAL INTCPT
      REAL SLOPE
      REAL INTCPTSIG
      REAL SLOPESIG
      REAL XMEAN
      REAL YMEAN
      REAL PCORR

*  STATUS:
      INTEGER STATUS             ! Global status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Deliver a summary of the fit results to the user.
*  First the X-axis data.
      IF ( WEIGHT ) THEN
         CALL MSG_OUT( ' ', 'Weighted least squares fit to a ' //
     :                 'straight line.', STATUS )
         CALL MSG_SETC( 'WTEXT', 'Weighted statistics' )
      ELSE
         CALL MSG_OUT( ' ', 'Unweighted least squares fit to a ' //
     :                 'straight line.', STATUS )
         CALL MSG_SETC( 'WTEXT', 'Statistics' )
      END IF

      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', '^WTEXT of the X-axis data:', STATUS )
      CALL MSG_SETI( 'NDAT', NDAT )
      CALL MSG_OUT( ' ', '   ^NDAT data points.', STATUS )
      CALL MSG_SETR( 'XMIN', XMIN )
      CALL MSG_OUT( ' ', '   Minimum X value = ^XMIN', STATUS )
      CALL MSG_SETR( 'XMAX', XMAX )
      CALL MSG_OUT( ' ', '   Maximum X value = ^XMAX', STATUS )
      CALL MSG_SETR( 'XMEAN', XMEAN )
      CALL MSG_OUT( ' ', '   Mean X value = ^XMEAN', STATUS )
      CALL MSG_SETR( 'XSIG', XSIG )
      CALL MSG_OUT( ' ',
     :              '   Standard deviation of the X-axis data = ^XSIG',
     :              STATUS )

*  The Y-axis data.
      IF ( WEIGHT ) THEN
         CALL MSG_SETC( 'WTEXT', 'Weighted statistics' )
      ELSE
         CALL MSG_SETC( 'WTEXT', 'Statistics' )
      END IF

      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', '^WTEXT of the Y-axis data:', STATUS )
      CALL MSG_SETI( 'NDAT', NDAT )
      CALL MSG_OUT( ' ', '   ^NDAT data points.', STATUS )
      CALL MSG_SETR( 'YMIN', YMIN )
      CALL MSG_OUT( ' ', '   Minimum Y value = ^YMIN', STATUS )
      CALL MSG_SETR( 'YMAX', YMAX )
      CALL MSG_OUT( ' ', '   Maximum Y value = ^YMAX', STATUS )
      CALL MSG_SETR( 'YMEAN', YMEAN )
      CALL MSG_OUT( ' ', '   Mean Y value = ^YMEAN', STATUS )
      CALL MSG_SETR( 'YSIG', YSIG )
      CALL MSG_OUT( ' ',
     :              '   Standard deviation of the Y-axis data = ^YSIG',
     :              STATUS )

*  The fit results.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', 'Fit results:', STATUS )
      CALL MSG_SETR( 'INTCPT', INTCPT )
      CALL MSG_SETR( 'INTCPTSIG', INTCPTSIG )
      CALL MSG_OUT( ' ', '   Intercept = ^INTCPT +/- ^INTCPTSIG',
     :              STATUS )

      CALL MSG_SETR( 'SLOPE', SLOPE )
      CALL MSG_SETR( 'SLOPESIG', SLOPESIG )
      CALL MSG_OUT( ' ', '   Slope = ^SLOPE +/- ^SLOPESIG', STATUS )

      CALL MSG_SETR( 'PCORR', PCORR )
      CALL MSG_OUT( ' ', '   Pearson correlation coefficient = ^PCORR',
     :              STATUS )

      END
* $Id$
