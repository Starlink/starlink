      SUBROUTINE CCD1_HISP( HIST, NBIN, ZERO, WIDTH, PERCEN, VALUE,
     :                      STATUS )
*+
*  Name:
*     CCD1_HISP

*  Purpose:
*     Locates a percentile value in a histogram.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_HISP( HIST, NBIN, ZERO, WIDTH, PERCEN, VALUE, STATUS )

*  Description:
*     The routine locates the value which corresponds to a given
*     percentile value in a histogram. The value returned is an
*     interpolation of the bins around the given position.

*  Arguments:
*     HIST( NBIN ) = INTEGER (Given)
*        The histogram of counts.
*     NBIN = INTEGER (Given)
*        The number of bins in the histogram.
*     ZERO = DOUBLE PRECISION (Given)
*        The zero point of the values which actually correspond to the
*        histogram bins.
*     WIDTH = DOUBLE PRECISION (Given)
*        The width of a bin in data values.
*     PERCEN = DOUBLE PRECISION (Given)
*        The percentile position in the data which is to be evaluated.
*     VALUE = DOUBLE PRECISION (Returned)
*        The interpolated percentile value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The histogram bins are related to the original values by
*
*         ( BIN_NUMBER - 1 ) * WIDTH + ZERO

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-NOV-1992 (PDRAPER):
*        Original version.
*     17-AUG-2000 (MBT):
*        Fixed a bug which prevented any result being generated when
*        PERCEN = 1.0.
*     {enter_changes_here}

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
      DOUBLE PRECISION ZERO
      DOUBLE PRECISION WIDTH
      DOUBLE PRECISION PERCEN

*  Arguments Returned:
      DOUBLE PRECISION VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION NEED      ! Count value that corresponds to
                                 ! percentile position
      DOUBLE PRECISION SUM       ! Sum of counts in histogram
      INTEGER I                  ! Loop variable
      DOUBLE PRECISION LAST      ! Last bin count

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Sum the counts in the histogram.
      SUM = 0.0D0
      DO 1 I = 1, NBIN
         SUM = SUM + DBLE( HIST( I ) )
 1    CONTINUE

*  Calculate how many counts the percentile position corresponds to.
      NEED = SUM * PERCEN

*  Re-initialise the sum of counts.
      SUM = 0.0D0

*  Initialise last bin count sum.
      LAST = 0.0D0

*  Loop until the count sum exceeds the sum which is required. The
*  previous bin and this bin are used to interpolate the actual
*  bin fraction which corresponds to the required percentile.
      DO 2 I = 1, NBIN
         SUM = SUM + DBLE( HIST( I ) )
         IF ( SUM .GE. NEED ) THEN

*  Located the position. Interpolate value from the last position
*  to this position, to get fractional bin offset.
            VALUE = ( NEED - LAST ) / ( SUM - LAST )

*  Determine the actual data value this position corresponds to.
            VALUE = ( DBLE( I - 1 ) + VALUE ) * WIDTH + ZERO
            GO TO 3
         END IF

*  Remember this count for use in interpolation.
         LAST = SUM
 2    CONTINUE
 3    CONTINUE

      END
* $Id$
