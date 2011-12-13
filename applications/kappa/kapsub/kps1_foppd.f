      SUBROUTINE KPS1_FOPPD( M, N, A, B, STATUS )
*+
*  Name:
*     KPS1_FOPPx

*  Purpose:
*     Converts data held in complex form (seperate real and imaginary
*     parts) to power and phase form.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_FOPPx( M, N, A, B, STATUS )

*  Arguments:
*     M = INTEGER (Given)
*        Number of pixels per line of the FFT.
*     N = INTEGER (Given)
*        Number of lines in the FFT.
*     A( M, N ) = ? (Given & Returned)
*        On input it is the real part of the FFT.
*        On output it is `power' = modulus of complex value.
*     B( M, N ) = ? (Given & Returned)
*        On input it is the imaginary part of the FFT.
*        On output it is `phase' = arctan(imag/real) in radians.
*     STATUS = INTEGER(Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for real and double-precision floating-
*     point data: replace "x" in the routine name by D or R as
*     appropriate.  The arrays supplied to the routine must have the
*     data type specified.

*  Algorithm:
*     - The input complex data is overwritten by the power and phase
*       data. `Power' is here used to refer to the modulus of the
*       complex input value, and phase is arctan(imaginary/real) in
*       radians.

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
*        Converted to KAPPA style and reordered the arguments.
*     9-JAN-1995 (DSB):
*        Converted to double precision.  Re-format in edstar-style.
*     1995 March 29 (MJC):
*        Used the modern style of variable declaration, and minor
*        stylistic changes.
*     1995 March 30 (MJC):
*        Made generic from POWPHA.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER M
      INTEGER N

*  Arguments Given and Returned:
      DOUBLE PRECISION A( M, N )
      DOUBLE PRECISION B( M, N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      DOUBLE PRECISION IM                 ! Imaginary value
      INTEGER J                  ! Pixel count
      INTEGER K                  ! Line count
      DOUBLE PRECISION PHASE              ! Phase value
      DOUBLE PRECISION POWER              ! Power value
      DOUBLE PRECISION RL                 ! Real value

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Just do it a pixel at a time.
      DO K = 1, N

         DO J = 1, M

            RL = A( J, K )
            IM = B( J, K )

            POWER = SQRT( RL * RL + IM * IM )

            IF ( IM .NE. 0.0D0 .OR. RL .NE. 0.0D0 ) THEN
               PHASE = ATAN2( IM, RL )
            ELSE
               PHASE = 0.0D0
            END IF

*  Overwrite the real and imaginary parts of the image with the power
*  and phase.
            A( J, K ) = POWER
            B( J, K ) = PHASE

         END DO

      END DO

      END
