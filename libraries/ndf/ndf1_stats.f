      SUBROUTINE NDF1_STATS( DIM, DATA, MAXV, MINV, MEAN, SIGMA, RMS,
     :                       NGOOD, STATUS )
*+
*  Name:
*     NDF1_STATS

*  Purpose:
*     Get simple statistics for a vectorised array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL NDF1_STATS( DIM, DATA, MAXV, MINV, MEAN, SIGMA, RMS,
*                       NGOOD, STATUS )

*  Description:
*     The routine returns simple statistics for the supplied
*     1-dimensional array.
*
*  Arguments:
*     DIM = INTEGER (Given)
*        Length of the array.
*     DATA( DIM ) = DOUBLE PRECISION (Given)
*        The array.
*     MAXV = DOUBLE PRECISION (Returned)
*        The maximum value in the array.
*     MINV = DOUBLE PRECISION (Returned)
*        The minimum value in the array.
*     MEAM = DOUBLE PRECISION (Returned)
*        The mean value in the array.
*     SIGMA = DOUBLE PRECISION (Returned)
*        The standard deviation of the values in the array.
*     RMS = DOUBLE PRECISION (Returned)
*        The root-mean-squared value in the array.
*     NGOOD = INTEGER (Returned)
*        The number of good values in the array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - VAL__BADD is returned for all values if there are no good
*     values in the array, or if an error has already occurred.

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     21-MAY-2015 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitive data constants

*  Arguments Given:
      INTEGER DIM
      DOUBLE PRECISION DATA( DIM )

*  Arguments Returned:
      DOUBLE PRECISION MAXV
      DOUBLE PRECISION MINV
      DOUBLE PRECISION MEAN
      DOUBLE PRECISION SIGMA
      DOUBLE PRECISION RMS
      INTEGER NGOOD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for array elements

*.

*  Initialise error return values.
      MAXV = VAL__BADD
      MINV = VAL__BADD
      MEAN = VAL__BADD
      SIGMA = VAL__BADD
      RMS = VAL__BADD

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise running sums, etc.
      MAXV = VAL__MIND
      MINV = VAL__MIND
      MEAN = 0.0D0
      SIGMA = 0.0D0
      RMS = 0.0D0
      NGOOD = 0

* Check each value.
      DO I = 1, DIM
         IF( DATA( I ) .NE. VAL__BADD ) THEN
            IF( DATA( I ) .GT. MAXV ) MAXV = DATA( I )
            IF( DATA( I ) .LT. MINV ) MINV = DATA( I )
            NGOOD = NGOOD + 1
            RMS = RMS + DATA( I )*DATA( I )
            MEAN = MEAN + DATA( I )
         END IF
      END DO

*  Calculate the returned values
      IF( NGOOD .GT. 0 ) THEN
         MEAN = MEAN/NGOOD
         RMS = RMS/NGOOD
         SIGMA = SQRT( RMS - MEAN*MEAN )
         RMS = SQRT( RMS )
      ELSE
         MAXV = VAL__BADD
         MINV = VAL__BADD
         MEAN = VAL__BADD
         SIGMA = VAL__BADD
         RMS = VAL__BADD
      END IF

      END
