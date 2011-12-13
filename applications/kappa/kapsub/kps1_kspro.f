      REAL FUNCTION KPS1_KSPRO( DIST, STATUS )
*+
*  Name:
*     PROBKS

*  Purpose:
*     Returns the probability of a Kolmogorov-Smirnov statistic.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     PROB = KPS1_KSPRO( DIST, STATUS )

*  Arguments:
*     DIST = REAL (Given)
*        Measure of separation of two cumulative distributions.
*     STATUS = INTEGER (Given)
*        The global status.

*  Returned Value:
*     KPS1_KSPRO = REAL
*       The probability the samples were identical.

*  References:
*     - Press et al., 1992, "Numerical Recipes in FORTRAN", 2nd edition
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
*       Original Starlink version.
*     1997 May 13 (MJC):
*        Added prologue and STATUS argument.  Renamed from PROBKS.
*        Some tidying.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      REAL DIST                  ! Dimensionless distance

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL EPS1                  ! Required precision
      PARAMETER ( EPS1 = 0.001 )

      INTEGER MXITER             ! Maximum number of iterations (20
                                 ! should be adequate for most purposes)
      PARAMETER ( MXITER = 100 )

*  Local Variables:
      REAL D2                    ! Work variable
      REAL FLIP                  ! Alternating sign
      INTEGER J                  ! Loop counter
      REAL LAST                  ! Last term
      REAL TERM                  ! Term in the sum of the probability

*.

*  Initialise the function.
      KPS1_KSPRO = 0.0

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise loop variables.
      D2 = -2.0 * DIST * DIST
      FLIP = 2.0
      LAST = 0.0

*  Begin loop.  In most cases 20 iterations should be adequate.
      DO J = 1, MXITER

*  Form the sum
         TERM = FLIP * EXP( D2 * REAL( J * J ) )
         KPS1_KSPRO = KPS1_KSPRO + TERM

*  Return if term is smaller than the precision.
         IF ( ABS( TERM ) .LE. EPS1 * LAST .OR.
     :        ABS( TERM ) .LE. VAL__EPSR * KPS1_KSPRO ) THEN
            GOTO 999
         END IF

*  Flip the coefficient in the next term.
         FLIP = -FLIP

*  Record the last term.
         LAST = ABS( TERM )

      END DO

*  Failed to converge.
      KPS1_KSPRO = 1.0

  999 CONTINUE

      END
