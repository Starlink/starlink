      SUBROUTINE KPG1_WTM3R( ORDDAT, WEIGHT, VAR, NENT, COVAR, RESULT,
     :                       RESVAR, STATUS )
*+
*  Name:
*     KPG1_WTM3R

*  Purpose:
*     Forms the weighted median of a list of ordered data values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WTM3R( ORDDAT, WEIGHT, VAR, NENT, COVAR, RESULT, RESVAR,
*                      STATUS )

*  Description:
*     This routine finds a value which can be associated with the half-
*     weight value. It sums all weights then finds a value for the
*     half-weight. The comparison with the half-weight value proceeds
*     in halves of the weights for each data point (half of the first
*     weight, then the second half of the first weight and the first
*     half of the second weight etc.) until the half weight is
*     exceeded. The data values around this half weight position are
*     then found and a linear interpolation of these values is the
*     weighted median. This routine also uses the order statistic
*     covariance array (for a population NENT big) to estimate the
*     change in the variance from a optimal measurement from the
*     given population, returning the adjusted variance.

*  Arguments:
*     ORDDAT( NENT ) = REAL (Given)
*        The list of ordered data for which the weighted median is
*        required
*     WEIGHT( NENT ) = REAL (Given)
*        The weights of the values.
*     VAR( NSET ) = REAL (Given)
*        The variance of the unordered sample now ordered in ARR.
*     NENT = INTEGER (Given)
*        The number of entries in the data array.
*     COVAR( * ) = DOUBLE PRECISION (Given)
*        The packed variance-covariance matrix of the order statistics
*        from a normal distribution of size NENT.
*     RESULT = REAL (Returned)
*        The weighted median
*     RESVAR = REAL (Returned)
*        The variance of result.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     - The input data must be ordered increasing. No BAD values may be
*     present.

*  Notes:
*     - The routine should only be used at real or better precisions.

*  Copyright:
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     RFWS: Rodney Warren-Smith (Durham University)
*     PDRAPER: Peter Draper (STARLINK)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     - (RFWS):
*        Original version.
*     5-APR-1991 (PDRAPER):
*        Changed to use preordered array, changed to generic routine.
*        Added prologue etc.
*     30-MAY-1991 (PDRAPER):
*        Added used array.
*     27-MAY-1992 (PDRAPER):
*        Added direct variance estimates.
*     7-MAY-2000 (DSB):
*        Brought into KAPPA. Removed argument USED. Made RESULT, VAR and
*        RESVAR type REAL instead of DOUBLE PRECISION.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER NENT
      REAL ORDDAT( NENT )
      REAL WEIGHT( NENT )
      REAL VAR
      DOUBLE PRECISION COVAR( * )

*  Arguments Returned:
      REAL RESULT
      REAL RESVAR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION D1        !
      DOUBLE PRECISION D2        ! Values around half weight
      DOUBLE PRECISION DRES      ! Double precision result
      DOUBLE PRECISION DVAR      ! Double precision variance
      DOUBLE PRECISION IW        ! Dummy
      DOUBLE PRECISION JW        ! Dummy
      DOUBLE PRECISION TOTWT     ! The total value of weights
      DOUBLE PRECISION VSUM      ! Covariance sum
      DOUBLE PRECISION W1        ! Fractional weight of value
      DOUBLE PRECISION W2        ! Fractional weight of value
      DOUBLE PRECISION WTINC     ! Increment in current wtsum
      DOUBLE PRECISION WTSUM     ! Current sum of weights
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER K                  ! Loop variable
      INTEGER LBND               ! Lower sum bound
      INTEGER UBND               ! Upper sum bound

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If only one input value has been given, do thing quickly.
      IF ( NENT .EQ. 1 ) THEN

*  Just one value, copy the input values to the output values.
         RESULT = ORDDAT( 1 )
         RESVAR = VAR
      ELSE

*  More than one value so process in earnest.
*  Sum weights.
         TOTWT = 0.0D0
         DO 1 I = 1, NENT
            TOTWT = TOTWT + DBLE( WEIGHT( I ) )
 1       CONTINUE

* Search for median weight.
         TOTWT = TOTWT * 0.5D0
         WTSUM = 0.0D0
         DO 2 I = 1, NENT
            IF ( I .EQ. 1 ) THEN
               WTINC = DBLE( WEIGHT( I ) ) * 0.5D0
            ELSE
               WTINC = ( DBLE( WEIGHT( I ) + WEIGHT( I - 1 ) ) )
     :                 *  0.5D0
            END IF
            WTSUM = WTSUM + WTINC
            IF ( WTSUM .GT. TOTWT ) GO TO 66
 2       CONTINUE
 66      I = MIN( I, NENT )

*  Bounds are present value and previous one.
         LBND = I - 1
         UBND = I
         D1 = DBLE( ORDDAT( LBND ) )
         D2 = DBLE( ORDDAT( I ) )

*  Set weights factors
         W1 = ( WTSUM - TOTWT ) / MAX( WTINC, 1.0D-20 )
         W2 = 1.0D0 - W1

* Interpolate between data values
         DRES = D1 * W1 + D2 * W2
         IF( DRES .GT. VAL__MINR .AND. DRES .LT. VAL__MAXR ) THEN
            RESULT = REAL( DRES )
         ELSE
            RESULT = VAL__BADR
         END IF

*  Sum the relevant ordered statistic variances and covariances.
*  weighting accordingly.
         VSUM = 0.0D0
         DO 3 K = LBND, UBND
            IF( K .EQ. LBND ) THEN
               IW = W1
            ELSE
               IW = W2
            END IF
            DO 4 J = K, UBND
               IF( J .EQ. LBND ) THEN
                  JW = W1
               ELSE
                  JW = W2
               END IF

*  Sum variances and twice covariances ( off diagonal elements ).
               IF( K .EQ. J ) THEN
                  VSUM = VSUM + IW * JW * COVAR( K + J * ( J - 1 )/ 2 )
               ELSE
                  VSUM = VSUM +
     :                   2.0D0 * IW * JW * COVAR( K + J * ( J - 1 )/ 2 )
               END IF
 4          CONTINUE
 3       CONTINUE

*  Right make the new variance estimate. Use the sum of variances
*  and covariances of the order statistic of the `trimmed' sample size
*  Sample variance changes to NENT * VAR to represent total variance
*  of original data.
         DVAR = VAR * NENT * VSUM
         IF( DVAR .GT. VAL__MINR .AND. DVAR .LT. VAL__MAXR ) THEN
            RESVAR = REAL( DVAR )
         ELSE
            RESVAR = VAL__BADR
         END IF

      END IF

      END
