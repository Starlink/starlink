      SUBROUTINE KPG1_HSSTP8( NUMBIN, HIST, VALMAX, VALMIN,
     :                        SUM, MEAN, MEDIAN, MODE, STATUS )
*+
*  Name:
*     KPG1_HSSTP8

*  Purpose:
*     Calculates statistics from an histogram.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_HSSTP8( NUMBIN, HIST, VALMAX, VALMIN, SUM, MEAN,
*                       MEDIAN, MODE, STATUS )

*  Description:
*     This routine calculates certain statistical parameters for
*     an histogram.  The median is derived by linear interpolation
*     within an histogram bin. The mode is estimated from 3*median -
*     2*mean, which is only valid for moderately skew distributions.

*  Arguments:
*     NUMBIN = INTEGER (Given)
*        Number of bins in the histogram.
*     HIST( NUMBIN ) = INTEGER*8 (Given)
*        The histogram whose statistics are to be derived.
*     VALMAX = DOUBLE PRECISION (Given)
*        Maximum data value included in the histogram.
*     VALMIN = DOUBLE PRECISION (Given)
*        Minimum data value included in the histogram.
*     SUM = DOUBLE PRECISION (Returned)
*        Sum of all values in the histogram.
*     MEAN = DOUBLE PRECISION (Returned)
*        Mean of the histogram.
*     MEDIAN = DOUBLE PRECISION (Returned)
*        Median of the histogram.
*     MODE = DOUBLE PRECISION (Returned)
*        Mode of the histogram.  The bad value (VAL__BADD) is returned
*        if the calculated mode lies outside the data range of the
*        histogram.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Algorithm:
*     -  Find sum and mean of the data using d.p. counters.
*     -  Find median and estimate mode via 3*median - 2*mean.

*  References:
*     Moroney, M.J., 1957, "Facts from Figures" (Pelican)
*     Goad, L.E. 1980, in "Applications of Digital Image Processing to
*       Astronomy", SPIE 264, 139.

*  Copyright:
*     Copyright (C) 2021 East Asian Observatory
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     15-JAN-2021 (DSB):
*        Original version based on KPG1_HSSTP.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE           ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Standard SAE constants
      INCLUDE 'PRM_PAR'        ! VAL__ constants

*  Arguments Given:
      INTEGER NUMBIN
      INTEGER*8 HIST( NUMBIN )
      DOUBLE PRECISION VALMAX
      DOUBLE PRECISION VALMIN

*  Arguments Returned:
      DOUBLE PRECISION SUM
      DOUBLE PRECISION MEAN
      DOUBLE PRECISION MEDIAN
      DOUBLE PRECISION MODE

*  Status:
      INTEGER  STATUS          ! Global status

*  Local Variables:
      DOUBLE PRECISION BINSZ   ! Bin width
      DOUBLE PRECISION HALFNM  ! Counter for middle point in image
      INTEGER I                ! General array counter
      INTEGER J                ! General array counter
      DOUBLE PRECISION NUMBER  ! Counter for number of points in image

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Evaluate the binsize.
      BINSZ = ( VALMAX - VALMIN ) / DBLE( NUMBIN )

*  Sum the number of values and derive the average bin number.
      NUMBER = 0.0D0
      SUM = 0.0D0

      DO  I = 1, NUMBIN
         SUM = SUM + ( DBLE( I ) * DBLE( HIST( I ) ) )
         NUMBER = NUMBER + DBLE( HIST( I ) )
      END DO

*  Form the approximate sum of the data.  Note that the bin centre
*  corresponds to the average value for a bin, so convert from bin
*  index to derive the sum.
      SUM =  ( NUMBER * VALMIN ) + ( SUM - 0.5D0 * NUMBER ) * BINSZ

*  Derive the approximate mean of the data.
      MEAN  =  SUM / NUMBER

*  Now find median and estimate mode. It is assumed that the histogram
*  bins are narrow enough that the error in calculating the sum of the
*  pixel values directly from the histogram bins with linear
*  interpolation is negligible.
      HALFNM = 0.0D0
      J = 1

*  The median is derived by looking for the halfway numbered pixel, and
*  calculating its corresponding data value.
      DO WHILE( ( HALFNM .LT. ( NUMBER / 2.0 ) ) .AND.
     :          ( J .LE. NUMBIN ) )
         HALFNM = HALFNM + DBLE( HIST( J ) )
         J = J + 1
      END DO

*  The median is determined by linear interpolation within the bin
*  ((J-1)th) where it is located, plus the sum to the previous bin.
      MEDIAN = VALMIN + BINSZ * ( DBLE( J-1 ) +
     :         ( 0.5D0 * NUMBER - HALFNM ) / DBLE( HIST( J-1 ) ) )

*  The mode will be calculated from the approximation:
*       mode = 3*median - 2*mean
*  which assumes only slightly skewed distributions.
      MODE = ( 3.0 * MEDIAN ) - ( 2.0 * MEAN )

*  The mode is clearly undefined when it lies outside the range of the
*  histogram.
      IF ( MODE .LT. VALMIN .OR. MODE .GT. VALMAX ) MODE = VAL__BADD

      END
