      SUBROUTINE KPS1_HSCEM( NUMBIN, HIST, MAXPOS, FRAC, MODEI,
     :                       STATUS )
*+
*  Name:
*     KPS1_HSCEM

*  Purpose:
*     Determines the histogram index of the mode by moments

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_HSCEM( NUMBIN, HIST, MAXPOS, FRAC, MODEI, STATUS )

*  Description:
*     This subroutine estimates the bin index of the mode using an
*     optimally binned histogram.  It finds the weighted mean
*     location of a the peak bin and its neighbours using moments.
*     The extents of the neighbours is symmetric about the peak bin.

*  Arguments:
*     NUMBIN = INTEGER (Given)
*        The number of histogram bins.
*     HIST( NUMBIN ) = INTEGER*8 (Given)
*        The optimally binned histogram.
*     MAXPOS = INTEGER (Given)
*        The bin index of the peak bin within HIST.
*     FRAC = REAL (Given)
*        This sets the lower limit for contiguous bin populations to
*        be regarded as a neighbour, and hence used to estimate the
*        mode by moments.

*        If FRAC is positive, it is the fraction of the peak bin's
*        population.  Positive values should lie in the range 0.1
*        to 0.9, otherwise a value of -2 is adopted.
*
*        If FRAC is negative, it is number of Poisson errors of
*        the peak's population.
*     MODEI = DOUBLE PRECISION (Returned)
*        The mode index estimated from the histogram peak.
*     STATUS = INTEGER (Given)
*        The interited status.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 June 29 (MJC):
*       Original version.
*     20-FEB-2020 (DSB):
*       Support huge arrays.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE definitions
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Arguments Given:
      INTEGER NUMBIN
      INTEGER*8 HIST( NUMBIN )
      INTEGER MAXPOS
      REAL FRAC

*  Arguments Returned:
      DOUBLE PRECISION MODEI

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER MINBHW             ! Minimum neighbourhood width
      PARAMETER( MINBHW = 1 )

*  Local Variables:
      INTEGER HALF               ! Neighbourhood half width
      INTEGER I                  ! Loop counter
      INTEGER LBIN               ! Neighbourhood lower bin index
      REAL LFRAC                 ! Local, possible modified, FRAC
      DOUBLE PRECISION SUM       ! Sum of the weighted histogram index
      DOUBLE PRECISION SUMW      ! Sum of the weights
      INTEGER*8 THRESH           ! Neighbourhood population threshold
      INTEGER UBIN               ! Neighbourhood upper bin index

*.

*  Initialise the mode.
      MODEI = 0

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the threshold level by first obtaining the fractional
*  population.
      LFRAC = FRAC
      IF ( ( FRAC .GE. 0.0 .AND. FRAC .LT. 0.1 ) .OR.
     :     FRAC .GT. 0.9 ) LFRAC = -2.0

      IF ( LFRAC .LT. 0 ) THEN
         LFRAC = 1.0 + FRAC / SQRT( REAL( HIST( MAXPOS ) ) )
      END IF
      THRESH = INT( REAL( HIST( MAXPOS ) ) * LFRAC, KIND=8 )

*   Look for a contiguous set of bins no lower than the threshold
*   either side of the peak bin.
      LBIN = MAXPOS - 1
      DO WHILE ( LBIN .GT. 0 .AND. HIST( LBIN ) .GE. THRESH )
         LBIN = LBIN - 1
      END DO
      LBIN = LBIN + 1

      UBIN = MAXPOS + 1
      DO WHILE ( UBIN .LE. NUMBIN .AND. HIST( UBIN ) .GE. THRESH )
         UBIN = UBIN + 1
      END DO
      UBIN = UBIN - 1

*  Make the neighbourhood symmetric about the peak bin but ensure there
*  are neighbours included unless the peak is near an end of the
*  histogram.
      HALF = MAX( MINBHW, MIN( UBIN - MAXPOS, MAXPOS - LBIN ) )

*  Redefine the bin limits.
      LBIN = MAXPOS - HALF
      UBIN = MAXPOS + HALF

*  Trim neighbourhood near the histogram extremes.
      HALF = HALF + MIN( 0, LBIN - 1 ) + MIN( 0, NUMBIN - UBIN )

*  Find the weight bin number corresponding to the mode.  Yes there
*  can be a bias, but it's likely to be small.  [Could do some
*  experiments to determine sensible ranges of bins to incorporate.
*  balancing the bias against skewness.]
      SUMW = 0.0D0
      SUM = 0.0D0
      DO I = MAXPOS - HALF, MAXPOS + HALF
         SUM = SUM + DBLE( I ) * DBLE( HIST( I ) )
         SUMW = SUMW + DBLE( HIST( I ) )
      END DO

      MODEI = SUM / SUMW

      END
