      SUBROUTINE CCD1_GAFIT( HIST, NBIN, MODE, SD, STATUS )
*+
*  Name:
*     CCD1_GAFIT

*  Purpose:
*     Fits a gaussian to histogram data with known peak.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GAFIT( HIST, NBIN, MODE, SD, STATUS )

*  Description:
*     The routine uses a logarithmic transformation of a histogram of
*     values to fit a gaussian whose peak position is known. This
*     routine just determines the standard deviation of the fit.
*     An iterative refinement of the fit is performed to reduce the
*     effects of non-gaussian outliers.

*  Arguments:
*     HIST( NBIN ) = INTEGER (Given)
*        The histogram of data values.
*     NBIN = INTEGER (Given)
*        The number of bins in the input histogram.
*     MODE = INTEGER (Given)
*        The bin position of the gaussian peak.
*     SD = DOUBLE PRECISION (Returned)
*        The standard deviation of the gaussian fit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The iterative refinement is performed three times. Each time
*     data outside of 80% of the peak count level are removed from the
*     sums.
*     -  The standard deviation of the gaussian is determined using
*     a logarithmic transformation of the equation:
*
*          y = Io * exp( -0.5* ( (x-Xo) /sigma ) **2 )
*
*     which breaks down to
*
*          Y = ln( Io ) + (X/sigma)**2
*
*     where Y = ln(y) and X=(x-Xo)
*
*     a least squares solution to this gives:
*
*          2*sigma**2 = S(X*X)/(n*C-S(Y))
*
*     where S() = sum of
*     n = number of values
*     C = ln(Io)

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-NOV-1992 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NBIN
      INTEGER HIST( NBIN )
      INTEGER MODE

*  Arguments Returned:
      DOUBLE PRECISION SD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION CHANGE
      DOUBLE PRECISION DENOM
      DOUBLE PRECISION FRAC
      DOUBLE PRECISION PEAK
      DOUBLE PRECISION LIMIT
      DOUBLE PRECISION LNPEAK
      DOUBLE PRECISION SDOLD
      DOUBLE PRECISION X
      DOUBLE PRECISION XSUM
      DOUBLE PRECISION Y
      DOUBLE PRECISION YSUM
      INTEGER IEND
      INTEGER I
      INTEGER J
      INTEGER NCOUNT
      INTEGER RANGE
      INTEGER START

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the peak count.
      PEAK = DBLE( HIST( MODE ) )

*  Take the natural logarithm.
      LNPEAK = LOG( PEAK )

*  Set fraction of SD to convert into 80% of peak intensity.
      FRAC = SQRT( -2.0D0 * LOG( 0.2D0 ) )

*  Set the range of bins to consider when determining fit. Look for the
*  nearest left and right bins with 20% of the number count of the
*  modal bin.
      LIMIT = 0.2D0 * PEAK
      START = 1
      IEND = NBIN
      DO 1 I = MODE, NBIN
         IF ( HIST( I ) .NE. 0 ) THEN
            IF ( HIST( I ) .LE. LIMIT ) THEN
               IEND = I
               GO TO 7
            END IF
         END IF
 1    CONTINUE
 7    CONTINUE
      DO 2 I = MODE, 1, -1
         IF ( HIST( I ) .NE. 0 ) THEN
            IF ( HIST( I ) .LE. LIMIT ) THEN
               START = I
               GO TO 8
            END IF
         END IF
 2    CONTINUE
 8    CONTINUE

*  Loop determining the gaussian standard deviation until the required
*  number of iterations has been performed or the standard deviation
*  is not changed by one bin.
      SDOLD = 0.0D0
      DO 3 I = 1, 3

*  Form the current estimate of the deviation.
         NCOUNT = 0
         YSUM = 0.0D0
         XSUM = 0.0D0
         DO 4 J = START, IEND
            IF ( HIST( J ) .NE. 0 ) THEN
               Y = DBLE( HIST( J ) )
               YSUM = YSUM + LOG( Y )
               X = DBLE( J - MODE )
               XSUM = XSUM + X * X
               NCOUNT = NCOUNT + 1
            END IF
 4       CONTINUE

*  Form the sigma estimate.
         DENOM = ( DBLE( NCOUNT ) * LNPEAK - YSUM )
         IF ( DENOM .NE. 0.0D0 ) THEN
            SD = XSUM / ( DBLE( NCOUNT ) * LNPEAK - YSUM )
            SD = SQRT( ABS( SD /2.0D0 ) )
         ELSE

*  Force another iteration.
            SD = SDOLD + 2.0D0
         END IF

*  Now look at change from last estimate.
         IF ( I .NE. 1 ) THEN
            CHANGE = SD - SDOLD
         ELSE
            CHANGE = 2.0D0
         END IF
         IF ( ABS( CHANGE ) .GT. 1.0D0 ) THEN

*  Try again. Set the range of bins so that 95% of the current gaussian
*  is used.
            RANGE = NINT( SD * FRAC )
            START = MAX( 1, MIN( ( MODE - RANGE ), NBIN ) )
            IEND = MAX( 1, MIN( ( MODE + RANGE ), NBIN ) )

*  Record the current SD estimate.
            SDOLD = SD
         ELSE

*  End the refinement.
            GO TO 5
         END IF

 3    CONTINUE
 5    CONTINUE

      END
* $Id$
