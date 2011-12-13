      SUBROUTINE KPS1_CNVRP( ILO, IHI, JLO, JHI, NPIX, NLIN, ARRAY,
     :                       STATUS )
*+
*  Name:
*     KPS1_CNVRP

*  Purpose:
*     Replicates edge pixels into the margins of an array, for
*     CONVOLVE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CNVRP( ILO, IHI, JLO, JHI, NPIX, NLIN, ARRAY, STATUS )

*  Description:
*     Replicates the edge values in a given subsection of the supplied
*     array out to the edges of the array.

*  Arguments:
*     ILO = INTEGER (Given)
*        The index of the column which is to be replicated to the left
*        hand edge (i.e. column 1) of the array.
*     IHI = INTEGER (Given)
*        The index of the column which is to be replicated to the right
*        hand edge (i.e. column NPIX) of the array.
*     JLO = INTEGER (Given)
*        The index of the row which is to be replicated to the bottom
*        edge (i.e. row 1) of the array.
*     JHI = INTEGER (Given)
*        The index of the row which is to be replicated to the top
*        edge (i.e. row NLIN) of the array.
*     NPIX = INTEGER (Given)
*        The number of columns in the array.
*     NLIN = INTEGER (Given)
*        The number of lines in the array.
*     ARRAY( NPIX, NLIN ) = DOUBLE PRECISION (Given and Returned)
*        The array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1995 (DSB):
*        Original version.
*     1995 March 22 (MJC):
*        Made some stylistic changes, removed long lines and used
*        modern-style variable declarations.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER ILO
      INTEGER IHI
      INTEGER JLO
      INTEGER JHI
      INTEGER NPIX
      INTEGER NLIN

*  Arguments Given and Returned:
      DOUBLE PRECISION ARRAY( NPIX, NLIN )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION VAL       ! Array value to be replicated
      INTEGER I                  ! Column index
      INTEGER J                  ! Row index

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each of the rows which intersect the given subsection of
*  the array.
      DO J = JLO, JHI

*  Replicate the left hand value in the subsection through the
*  left-hand border.
         VAL = ARRAY( ILO, J )
         DO I = 1, ILO - 1
            ARRAY( I, J ) = VAL
         END DO

*  Replicate the right hand value in the subsection through the
*  right-hand border.
         VAL = ARRAY( IHI, J )
         DO I = IHI + 1, NPIX
            ARRAY( I, J ) = VAL
         END DO

      END DO

*  Replicate the bottom line of the subsection (but including the left-
*  and right-hand border regions), through the bottom border.
      DO J = JLO - 1, 1, -1
         DO I = 1, NPIX
            ARRAY( I, J ) = ARRAY( I, J + 1 )
         END DO
      END DO


*  Replicate the top line of the subsection (but including the left-
*  and right-hand border regions), through the top border.
      DO J = JHI + 1, NLIN
         DO I = 1, NPIX
            ARRAY( I, J ) = ARRAY( I, J - 1 )
         END DO
      END DO

      END
