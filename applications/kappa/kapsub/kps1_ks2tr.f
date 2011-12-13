      SUBROUTINE KPS1_KS2TR( N1, N2, DATA1, DATA2, D, PROB, SORT1,
     :                         SORT2, STATUS )
*+
*  Name:
*     KPS1_KS2Tx

*  Purpose:
*     Returns the probability that two datasets are from the same
*     sample.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_KS2Tx( N1, N2, DATA1, DATA2, D, PROB, SORT1, SORT2, STATUS )

*  Description:
*     This routine computes the two-sided Kolmogorov-Smirnov statistic
*     of two arrays and returns the probability that the two arrays are
*     drawn from the same sample.

*  Arguments:
*     N1 = INTEGER (Given)
*        Number of elements in the first array.
*     N2 = INTEGER (Given)
*        Number of elements in the second array.
*     DATA1( N1 ) = ? (Given and Returned)
*        First input array.
*     DATA2( N2 ) = ? (Given and Returned)
*        Second input array.
*     D  = ? (Returned)
*        Maximum distance between cumulative distribution functions.
*     PROB = REAL (Returned)
*        Probability that the two supplied arrays are from the same
*        sample.
*     SORT1( N1 ) = ? (Returned)
*        Sorted form of array DATA1.
*     SORT2( N2 ) = ? (Returned)
*        Sorted form of array DATA2.
*     STATUS = INTEGER (Given & Returned)
*        Global status value.

*  Notes:
*     -  The input arrays must not contain any bad data.
*     -  There is a routine for double-precision, and real data types:
*     replace "x" in the routine name by D or R as appropriate.  The
*     DATA1, DATA2, SORT1, SORT2 arrays and the maximum distance
*     supplied to the routine must have the data type specified.

*  Algorithm:
*     This routine first sorts both arrays and then compares cumulative
*     distribution functions.  The largest separation between these
*     functions is then used to calculate the Kolmogorov-Smirnov
*     statistic.

*  References:
*     - Press et al, 1992, "Numerical Recipes in FORTRAN", 2nd edition
*     (CUP).

*  Copyright:
*     Copyright (C) 1996-1997 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     TIMJ: Tim Jenness (JACH)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 October 22 (TIMJ):
*       Original Starlink version
*     1997 May 13 (MJC):
*        Made generic and some tidying.  Reordered the N2 argument to
*        adhere to the Starlink standard.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions

*  Arguments Given:
      INTEGER N1
      INTEGER N2

*  Arguments Given and Returned:
      REAL DATA1( N1 )
      REAL DATA2( N2 )

*  Arguments Returned:
      REAL D
      REAL PROB
      REAL SORT1( N1 )
      REAL SORT2( N2 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL KPS1_KSPRO
      REAL     KPS1_KSPRO        ! KS statistic

*  Local Variables:
      REAL  D1                 ! Value from first set
      REAL  D2                 ! Value from second set
      REAL  DTEMP              ! Difference between FN2 and FN1
      REAL    EN                 ! Weighted number of points
      REAL    FRAC1              ! Fraction through dataset 1
      REAL    FRAC2              ! Fraction through dataset 2
      INTEGER I                  ! Loop counter
      INTEGER J1                 ! Counter
      INTEGER J2                 ! Counter
      REAL  R1                 ! REAL(N1)
      REAL  R2                 ! REAL(N2)

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy data to the scratch arrays.
      DO I = 1, N1
         SORT1( I ) = DATA1( I )
      END DO

      DO I = 1, N2
         SORT2( I ) = DATA2( I )
      END DO

*  Sort the data before further processing
      CALL KPG1_QSRTR( N1, 1, N1, SORT1, STATUS )
      CALL KPG1_QSRTR( N2, 1, N2, SORT2, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         PROB = -1.0
         D = 0.0E0
         GOTO 999
      END IF

*  Initialise variables.
      R1 = NUM_ITOR( N1 )
      R2 = NUM_ITOR( N2 )

      J1 = 1
      J2 = 1
      FRAC1 = 0.0E0
      FRAC2 = 0.0E0
      D = 0.0E0

*  Loop through the data.
      DO WHILE ( J1 .LE. N1 .AND. J2 .LE. N2 )

         D1 = SORT1( J1 )
         D2 = SORT2( J2 )

*  Move along an element in the first array.  Find the fractional
*  distance.
         IF ( D1 .LE. D2 ) THEN
            FRAC1 = NUM_ITOR( J1 ) / R1
            J1 = J1 + 1
         END IF

*  Move along an element in the second array.  Find the fractional
*  distance.
         IF ( D2 .LE. D1 ) THEN
            FRAC2 = NUM_ITOR( J2 ) / R2
            J2 = J2 + 1
         END IF

*  Find the distance between the points.
         DTEMP = ABS( FRAC2 - FRAC1 )
         IF ( DTEMP .GT. D ) D = DTEMP
      END DO

*  Find the KS statistic associated with maximum separation.
      EN = NUM_RTOR( SQRT( R1 * R2 / ( R1 + R2 ) ) )
      PROB = KPS1_KSPRO( ( EN + 0.12 + 0.11 / EN ) * NUM_RTOR( D ),
     :       STATUS )

  999 CONTINUE

      END
