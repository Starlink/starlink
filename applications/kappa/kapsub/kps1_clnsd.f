      SUBROUTINE KPS1_CLNSD( LBND, UBND, XCEN, X, STATUS )
*+
*  Name:
*     KPS1_CLNSx

*  Purpose:
*     To 'clean' a marginal star profile, removing neighbouring stars
*     and blemishes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CLNSx( LBND, UBND, XCEN, X, STATUS )

*  Description:
*     This routine cleans a marginal star profile by subtracting the
*     lower quartile data point as a background estimate.  It works
*     out from the centre of the star, preventing each data point from
*     exceeding the maximum of the two previous data values.  This
*     ensures that the data does not increase with increasing distance
*     from the centre.

*  Arguments:
*     LBND = INTEGER (Given)
*        The lower bound of the data array.
*     UBND = INTEGER (Given)
*        The upper bound of the data array.
*     XCEN = REAL (Given)
*        The estimated position of the star centre.
*     X( LBND:UBND ) = ? (Given and Returned)
*        The marginal data array.  On exit it is cleaned and background
*        subtracted.
*     STATUS = INTEGER (Given)
*        The global status.

*  Notes:
*     There is a routine for each numeric data type: replace "x" in the
*     routine name by D, R, I, W, UW, B, UB as appropriate.  The array
*     supplied to the routine must have the data type specified.

*  Copyright:
*     Copyright (C) 1981, 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (Durham Univ.)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1981 (RFWS):
*        Original version.
*     1990 September 18 (MJC):
*        Made generic, renamed from CLNSTA, re-ordered arguments,
*        commented the variables, added status, and converted the
*        prologue.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad-pixel constants

*  Arguments Given:
      INTEGER
     :  LBND,
     :  UBND

      REAL
     :  XCEN

*  Arguments Given and Returned:
      DOUBLE PRECISION
     :  X( LBND:UBND )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NMIN               ! Minimum number of previous data
                                 ! values that must not be exceeded
      PARAMETER ( NMIN = 2 )

*  Local Variables:
      INTEGER
     :  I, J,                    ! Loop counters
     :  IDIRN,                   ! Direction from the star centre
     :  IEND,                    ! Pixel index of the end of the
                                 ! profile away from the centre
     :  I0,                      ! Pixel index of the estimate of the
                                 ! star centre
     :  MINLOC,                  ! Pixel index of the current upper
                                 ! limit
     :  NQUART,                  ! Pixel index of the lower quartile
     :  NX                       ! Number of pixels

      DOUBLE PRECISION
     :  SMAX,                    ! Maximum background-corrected data
                                 ! value
     :  SMIN( NMIN ),            ! The last values
     :  STAK( 25 ),              ! The smallest values in the marginal
                                 ! data array
     :  UPLIM                    ! Upper data limit

*  Internal References:
      INCLUDE 'NUM_DEC'          ! NUM declarations
      INCLUDE 'NUM_DEF'          ! NUM definitions

*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Calculate the number of data points and find the lower quartile
*    point as a background estimate.

      NX = UBND - LBND + 1
      NQUART = MIN( MAX( 1, NINT( NX * 0.25 ) ), 25 )
      CALL KPG1_NTHMD( .TRUE., X( LBND ), NX, NQUART, STAK, STATUS )

*    Subtract the background from all the valid data points, finding

*    Find the maximum value.

      SMAX = VAL__MIND

      DO I = LBND, UBND

         IF ( X( I ) .NE. VAL__BADD ) THEN

*          This is safe because there can't be an overflow.

            X( I ) = X( I ) - STAK( 1 )
            IF ( NUM_GTD( X( I ), SMAX ) ) SMAX = X( I )
         END IF

      END DO

*    Work out from the centre of the star image towards each end
*    of the data array in turn.

      I0 = NINT( XCEN )

      DO IDIRN = -1, 1, 2

         IF ( IDIRN .LT. 0 ) THEN
            IEND = LBND
         ELSE
            IEND = UBND
         END IF

*       Initialise the store of the last NMIN data values.

         DO J = 1, NMIN
            SMIN( J ) = SMAX
         END DO

         MINLOC = 1

*       Work through the data array, at each point calculating an upper
*       data limit as the maximum of the last NMIN values.

         DO I = I0, IEND, IDIRN
            UPLIM = SMIN( 1 )

            DO J = 2, NMIN
               IF ( NUM_GTD( SMIN( J ), UPLIM ) ) UPLIM = SMIN( J )
            END DO

*          Limit the data to the current upper limit.

            IF ( NUM_GTD( X( I ), UPLIM ) ) X( I ) = UPLIM

*          MINLOC cycles from 1 to NMIN and points to the store for
*          this value in the list of the last NMIN values.

            MINLOC = MOD( MINLOC, NMIN ) + 1

            IF ( X( I ) .GE. 0.0 ) THEN
               SMIN( MINLOC ) = X( I )

            ELSE

*             Don't use negative values...replace with the current upper
*             limit.

               SMIN( MINLOC ) = UPLIM
            END IF

         END DO

      END DO

      END
