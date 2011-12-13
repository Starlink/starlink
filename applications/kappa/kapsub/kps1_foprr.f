      SUBROUTINE KPS1_FOPRR( NCOLI, NLINI, IN, NCOLO, NLINO, FILL,
     :                        ZERO, OUT, STATUS )
*+
*  Name:
*     KPS1_FOPRx

*  Purpose:
*     Prepares a 2-d array for being Fourier transformed.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_FOPRx( NCOLI, NLINI, IN, NCOLO, NLINO, FILL, ZERO, OUT,
*                      STATUS )

*  Description:
*     The input 2-d array is copied to an output array.  It replaces
*     invalid pixels by a prescribed value in the output.  If the input
*     array is not as big as the output, the output array is padded
*     with pixels with the same defined value.  If the input is larger
*     than the output only the pixels that lie within the output
*     array's bounds are copied.  If no input array is given, then
*     the output array is filled with zeros.

*  Arguments:
*     NCOLI = INTEGER (Given)
*        Number of columns per line of the input array.
*     NLINI = INTEGER (Given)
*        Number of lines in the input array.
*     IN( NCOLI, NLINI ) = ? (Given)
*        The array of data to be prepared.
*     NCOLO = INTEGER (Given)
*        Number of columns per line of the output array.
*     NLINO = INTEGER (Given)
*        Number of lines in the output array.
*     FILL = ? (Given)
*        The value with which to fill data `holes' or bad pixels.
*     ZERO = LOGICAL (Given)
*        If .TRUE., then output image is filled with zeros.
*     OUT( NCOLO, NLINO ) = ? (Returned)
*        The prepared array of data ready to be Fourier transformed.
*     STATUS = INTEGER (Given)
*        The global status.

*  Notes:
*     -  There is a routine for real and double-precision floating-
*     point data: replace "x" in the routine name by D or R as
*     appropriate.  The arrays and fill value supplied to the routine
*     must have the data type specified.

*  Algorithm:
*     - If the output is to be filled with zeroes then do so.
*     - Otherwise copy the input array, or a subset thereof, to the
*       output, replacing any bad pixels with the fill value.  Pad the
*       end of each line if necessary, and finally pad any remaining
*       lines in the output.

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
*     1988 Apr 18 (DSB):
*        Original version called PREPIM.
*     1990 Mar 19 (MJC):
*        Converted to KAPPA style and reordered the arguments.
*     1990 Mar 29 (MJC):
*        Improved the efficiency and allowed for trimming.
*     9-JAN-1995 (DSB):
*        Convert to double precision.  Re-format to edstar-style.
*     1995 March 29 (MJC):
*        Added prologue terminator, and used modern-style variable
*        declarations, and some other minor improvements to the
*        commentary.
*     1995 March 30 (MJC):
*        Made generic from FTPREP.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Magic-value constants

*  Arguments Given:
      REAL FILL
      INTEGER NCOLI
      INTEGER NLINI
      INTEGER NCOLO
      INTEGER NLINO
      REAL IN( NCOLI, NLINI )
      LOGICAL ZERO

*  Arguments Returned:
      REAL OUT( NCOLO, NLINO )

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER J                  ! Pixel count
      INTEGER K                  ! Line count
      LOGICAL PADLIN             ! Columns are to be padded at the end
                                 ! of each line?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Jump forward if output is to be filled with zeros.
      IF ( .NOT. ZERO ) THEN

         PADLIN = NCOLO .GT. NCOLI

*  Loop for each pixel in the unpadded or trimmed array.
         DO K = 1, MIN( NLINI, NLINO )
            DO J = 1, MIN( NCOLI, NCOLO )

*  The Fourier transform cannot handle the magic values so replace them
*  with the fill value.
               IF ( IN( J, K ) .EQ. VAL__BADR ) THEN
                  OUT( J, K ) = FILL

*  Just copy the value across to the padded array.
               ELSE
                  OUT( J, K ) = IN( J, K )

               END IF

            END DO

*  Extend the line where necessary.
            IF ( PADLIN ) THEN
               DO J = NCOLI + 1, NCOLO

*  Pad the the output array with the fill value.
                  OUT( J, K ) = FILL

               END DO
            END IF

         END DO

*  Now pad the remaining lines to their full number of columns in
*  the output array if there are any.
         IF ( NLINO .GT. NLINI ) THEN
            DO K = NLINI + 1, NLINO
               DO J = 1, NCOLO

*  Pad with the filling value.
                  OUT( J, K ) = FILL

               END DO
            END DO
         END IF

*  Now deal with case where output is filled with zeros.
      ELSE

         DO K = 1, NLINO
            DO J = 1, NCOLO
               OUT( J, K ) = 0.0E0
            END DO
         END DO

      END IF

      END
