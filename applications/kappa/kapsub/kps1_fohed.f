      SUBROUTINE KPS1_FOHED( M, N, R, I, H, MAXDEV, STATUS )
*+
*  Name:
*     KPS1_FOHEx

*  Purpose:
*     Produces a Hermitian FFT image from separate 2-d arrays holding
*     real and imaginary parts of the FFT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_FOHEx( M, N, R, I, H, MAXDEV, STATUS )

*  Description:
*     The real and imaginary arrays are `tangled' to form an Hermitian
*     matrix.

*  Arguments:
*     M = INTEGER (Given)
*        Number of pixels per line in each of the input arrays.
*     N = INTEGER (Given)
*        Number of lines in each of the input arrays.
*     R( M, N ) = ? (Given)
*        Input image holding the real data.
*     I( M, N ) = ? (Given)
*        Input image holding the imaginary data.
*     H( M, N ) = ? (Returned)
*        The Hermitian FFT.
*     MAXDEV = ? (Returned)
*        The maximum difference between estimates of the same Fourier
*        component.
*     STATUS = INTEGER (Given)
*        The global status.

*  Notes:
*     -  There is a routine for real and double-precision floating-
*     point data: replace "x" in the routine name by D or R as
*     appropriate.  The arrays and maximum difference supplied to
*     the routine must have the data type specified.

*  Algorithm:
*     - The `untangling' process described in routine FFTFOR is
*       reversed.
*     - Two separate estimates of each pixel value in the
*       Hermitian array are produced from the two input arrays.
*       These estimates should always be the same if the input real and
*       imaginary arrays correspond to a purely real array in image
*       space.
*     - If the estimates differ significantly then the user
*       has done something to the FFT which results in it no longer
*       being the FFT of a real image.
*     - If this is the case then the call application can not be used to
*       invert the FFT.  The user is told what the maximum deviation is
*       between any two estimates of the same pixel in the Hermitian
*       FFT.

*  Copyright:
*     Copyright (C) 1988, 1990 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     DSB: D.S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1988 Jun 6 (DSB):
*        Original version.
*     1990 Mar 14 (MJC):
*        Converted to KAPPA/ADAM, and replaced ILEVEL argument by
*        MAXDEV.
*     9-JAN-1995 (DSB):
*        Converted to double precision. Re-format in edstar-style.
*        Remove redundant include files.
*     1995 March 29 (MJC):
*        Used the modern style of variable declaration, and minor
*        stylistic changes.
*     1995 March 30 (MJC):
*        Made generic from HERMIT.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER M
      INTEGER N

      DOUBLE PRECISION I( M, N )
      DOUBLE PRECISION R( M, N )

*  Arguments Returned:
      DOUBLE PRECISION H( M, N )
      DOUBLE PRECISION MAXDEV

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FM                 ! Reflection constant for columns
      INTEGER FN                 ! Reflection constant for lines
      INTEGER J                  ! Column loop count
      INTEGER JLIM               ! Upper limit of column loop
      INTEGER K                  ! Line loop count
      INTEGER KLIM               ! Upper limit of line loop
      LOGICAL MEVEN              ! Number of columns is even?
      INTEGER MP1H               ! ( M+1 ) / 2
      INTEGER MP3H               ! ( M+3 ) / 2
      LOGICAL NEVEN              ! Number of lines is even?
      INTEGER NP1H               ! ( N+1 ) / 2
      INTEGER NP3H               ! ( N+3 ) / 2
      DOUBLE PRECISION V1                 ! First estimate
      DOUBLE PRECISION V2                 ! Second estimate

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get upper limits for the column and line loops.
      IF ( MOD( N, 2 ) .EQ. 0 ) THEN
         NEVEN = .TRUE.
         KLIM = N - 1
      ELSE
         NEVEN = .FALSE.
         KLIM = N
      END IF

      IF ( MOD( M, 2 ) .EQ. 0 ) THEN
         MEVEN = .TRUE.
         JLIM = M - 1
      ELSE
         MEVEN = .FALSE.
         JLIM = M
      END IF

*  Calculate often-used constants.
      MP1H = ( M + 1 ) / 2
      MP3H = ( M + 3 ) / 2
      FM = 2 * MP1H
      NP1H = ( N + 1 ) / 2
      NP3H = ( N + 3 ) / 2
      FN = 2 * NP1H

*  Do the lowest line in the top-left quadrant.
      MAXDEV = 0.0D0
      H( MP1H, NP1H ) = R( MP1H, NP1H )

      DO J = MP3H, JLIM

*  Get the mean and difference of the two real estimates about the
*  reflection column.  Store the Hermitian value in the top-left
*  quadrant.
         V1 = R( J, NP1H )
         V2 = R( FM - J, NP1H )
         H( J, NP1H ) = ( V1 + V2 ) / 2.0D0
         MAXDEV = MAX( MAXDEV, ABS( V2 - V1 ) )

*  Repeat for the imaginary data, but storing in the top-right-hand
*  quadrant.
         V1 = I( FM - J, NP1H )
         V2 = -I( J, NP1H )
         H( FM - J, NP1H ) = ( V1 + V2 ) / 2.0D0
         MAXDEV = MAX( MAXDEV, ABS( V2 - V1 ) )

      END DO

      IF ( MEVEN ) H( M, NP1H ) = R( M, NP1H )

*  Now repeat the above for the remaining lines in the top-left
*  quadrant which have matching lines in the bottom-left and
*  bottom-right quadrants.  This will leave one left over if the
*  number of lines is even.
      DO K = NP3H, KLIM

         V1 = R( MP1H, K )
         V2 = R( MP1H, FN - K )
         H( MP1H, K ) = ( V1 + V2 ) / 2.0D0
         MAXDEV = MAX( MAXDEV, ABS( V2 - V1 ) )

         V1 = I( MP1H, FN - K )
         V2 = -I( MP1H, K )
         H( MP1H, FN - K ) = ( V1 + V2 ) / 2.0D0
         MAXDEV = MAX( MAXDEV, ABS( V2 - V1 ) )

         DO J = MP3H, JLIM

            V1 = ( R( J, K ) + R( FM - J, K ) ) / 2.0D0
            V2 = ( R( J, FN - K ) + R( FM - J, FN - K ) ) / 2.0D0
            H( J, K ) = ( V1 + V2 ) / 2.0D0
            MAXDEV = MAX( MAXDEV, ABS( V2 - V1 ) )

            V1 = ( R( J, FN - K ) - R( FM - J, FN - K ) ) / 2.0D0
            V2 = ( R( FM - J, K ) - R( J, K ) ) / 2.0D0
            H( FM - J, FN - K ) = ( V1 + V2 ) / 2.0D0
            MAXDEV = MAX( MAXDEV, ABS( V2 - V1 ) )

            V1 = ( I( FM - J, K ) - I( J, K ) ) / 2.0D0
            V2 = ( I( FM - J, FN - K ) - I( J, FN - K ) ) / 2.0D0
            H( FM - J, K ) = ( V1 + V2 ) / 2.0D0
            MAXDEV = MAX( MAXDEV, ABS( V2 - V1 ) )

            V1 = ( - I( FM - J, K ) - I( J, K ) ) / 2.0D0
            V2 = ( I( FM - J, FN - K ) + I( J, FN - K ) ) / 2.0D0
            H( J, FN - K ) = ( V1 + V2 ) / 2.0D0
            MAXDEV = MAX( MAXDEV, ABS( V2 - V1 ) )

         END DO

*  Handle the remaining line in a similar fashion.
         IF ( MEVEN ) THEN

            V1 = R( M, K )
            V2 = R( M, FN - K )
            H( M, K ) = ( V1 + V2 ) / 2.0D0
            MAXDEV = MAX( MAXDEV, ABS( V2 - V1 ) )

            V1 = -I( M, K )
            V2 = I( M, FN - K )
            H( M, FN - K ) = ( V1 + V2 ) / 2.0D0
            MAXDEV = MAX( MAXDEV, ABS( V2 - V1 ) )

         END IF

      END DO

*  If the number of lines is even do the remaining unmatched top line
*  of the top-left quadrant.
      IF ( NEVEN ) THEN

         H( MP1H, N ) = R( MP1H, N )

         DO J = MP3H, JLIM

            V1 = R( J, N )
            V2 = R( FM - J, N )
            H( J, N ) = ( V1 + V2 ) / 2.0D0
            MAXDEV = MAX( MAXDEV, ABS( V2 - V1 ) )

            V1 = -I( J, N )
            V2 = I( FM - J, N )
            H( FM - J, N ) = ( V1 + V2 ) / 2.0D0
            MAXDEV = MAX( MAXDEV, ABS( V2 - V1 ) )

         END DO

         IF ( MEVEN ) H( M, N ) = R( M, N )

      END IF

      END
