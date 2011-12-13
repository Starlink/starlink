      SUBROUTINE KPS1_FOQUD( MO, NO, IN, MI, NI, SWAP, OUT, STATUS )
*+
*  Name:
*     KPS1_FOQUx

*  Purpose:
*     Copies one 2-d array to another optionally swapping quadrants.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_FOQUx( MO, NO, IN, MI, NI, SWAP, OUT, STATUS )

*  Arguments:
*     MI = INTEGER (Given)
*        The number of pixels per line in the unpadded region.
*     NI = INTEGER (Given)
*        The number of lines in the unpadded region.
*     IN( MO, NO ) = ? (Given)
*        The input image.
*     MO = INTEGER (Given)
*        The number of pixels per line in the whole image.
*     NO = INTEGER (Given)
*        The number of lines in the whole image.
*     SWAP = INTEGER (Given)
*        The type of quadrant swapping.  +1 gives a forward swap, while
*        -1 causes a reverse swap, and 0 gives no swapping.
*     OUT( MO, NO ) = ? (Returned)
*        The output image.
*     STATUS = INTEGER (Given)
*        The global status.

*  Notes:
*     -  There is a routine for real and double-precision floating-
*     point data: replace "x" in the routine name by D or R as
*     appropriate.  The arrays and fill value supplied to the routine
*     must have the data type specified.

*  Algorithm:
*     - The images may have been padded out at high column and
*       line numbers (see preprocessing subroutine), therefore the
*       centre of the image in not necessarily the centre of the
*       valid data.  This means that the process of swapping quadrants
*       one way, and swapping them back again, are not identical.
*     - The quadrants are swapped depending on the chosen option.  These
*       are `forward' which the swap is done around the centre of the
*       unpadded data, and `reverse' in which the swap is done about a
*       position equal to the `forward' position reflected through the
*       image centre.

*  Copyright:
*     Copyright (C) 1988, 1990, 1994 Science & Engineering Research
*     Council. Copyright (C) 1995 Central Laboratory of the Research
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
*     DSB: D.S. Berry (STARLINK) 6/6/88
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1988 June 6 (DSB):
*        Original version.
*     1990 Mar 15 (MJC):
*        Converted to KAPPA and ADAM involving a reorder of the
*        arguments and renamed from QUADS to KPS1_FOQUx.
*     9-JAN-1994 (DSB):
*        Convert to double precision.  Re-format to edstar style.
*     1995 March 29 (MJC):
*        Used the modern style of variable declaration, and minor
*        stylistic changes.
*     1995 March 30 (MJC):
*        Made generic from FTQUAD.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER MO
      INTEGER NO
      INTEGER MI
      INTEGER NI
      INTEGER SWAP
      DOUBLE PRECISION IN( MO, NO )

*  Arguments Returned:
      DOUBLE PRECISION OUT( MO, NO )

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER C1                 ! Number of columns to right of forward
                                 ! swap centre
      INTEGER C2                 ! Number of lines above forward swap
                                 ! centre
      INTEGER J                  ! Column count
      INTEGER K                  ! Line count
      INTEGER MIH                ! X (columns) co-ord of forward swap
                                 ! centre
      INTEGER NIH                ! Y (lines) co-ord of forward swap
                                 ! centre

*.

*  Check THE inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up constants.
      NIH = NI / 2
      MIH = MI / 2
      C1 = MO - MIH
      C2 = NO - NIH

*  If required forward swap the quadrants.
      IF ( SWAP .EQ. 1 ) THEN

         DO K = 1, NIH
            DO J = 1, MIH
               OUT( J + C1, K + C2 ) = IN( J, K )
            END DO

            DO J = MIH + 1, MO
               OUT( J - MIH, K + C2 ) = IN( J, K )
            END DO
         END DO

         DO K = NIH + 1, NO
            DO J = 1, MIH
               OUT( J + C1, K - NIH ) = IN( J, K )
            END DO

            DO J = MIH + 1, MO
               OUT( J - MIH, K - NIH ) = IN( J, K )
            END DO
         END DO

*  Alternatively, reverse swap the quadrants.
      ELSE IF ( SWAP .EQ.  -1 ) THEN

         DO K = 1, NIH
            DO J = 1, MIH
               OUT( J, K ) = IN( J + C1, K + C2 )
            END DO

            DO J = MIH + 1, MO
               OUT( J, K ) = IN( J - MIH, K + C2 )
            END DO
         END DO

         DO K = NIH + 1, NO
            DO J = 1, MIH
               OUT( J, K ) = IN( J + C1, K - NIH )
            END DO

            DO J = MIH + 1, MO
               OUT( J, K ) = IN( J - MIH, K - NIH )
            END DO
         END DO

*  Otherwise just copy input to output.
      ELSE
         DO K = 1, NO
            DO J = 1, MO
               OUT( J, K ) = IN( J, K )
            END DO
         END DO
      END IF

      END
