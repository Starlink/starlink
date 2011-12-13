      SUBROUTINE KPS1_LPLNM( FRM, IAXIS, DIM, NDIM, POS, AXVAL, BAD,
     :                       STATUS )
*+
*  Name:
*     KPS1_LPLNM

*  Purpose:
*     Normalise a set of axis values for LINPLOT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LPLNM( FRM, IAXIS, DIM, NDIM, POS, AXVAL, BAD, STATUS )

*  Description:
*     This routine normalises the supplied positions using the supplied
*     Frame, and then copies the value on axis IAXIS into the returned
*     array. A flag is returned if any bad values are found.

*  Arguments:
*     FRM = INTEGER (Given)
*        A pointer to the Frame in which the positions are defined.
*     IAXIS = INTEGER (Given)
*        The index of the Frame axis which is to be extracted and returned
*        in AXVAL.
*     DIM = INTEGER (Given)
*        The number of positions.
*     NDIM = INTEGER (Given)
*        The number of axes in FRM.
*     POS( DIM, NDIM ) = DOUBLE PRECISION (Given)
*        The positions.
*     AXVAL( DIM ) = DOUBLE PRECISION (Returned)
*        The normalised axis values.
*     BAD = LOGICAL (Returned)
*        Any bad values in returned array?
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER FRM
      INTEGER IAXIS
      INTEGER DIM
      INTEGER NDIM
      DOUBLE PRECISION POS( DIM, NDIM )

*  Arguments Returned:
      DOUBLE PRECISION AXVAL( DIM )
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION WORK( NDF__MXDIM )! Local work array
      INTEGER I                  ! Position index
      INTEGER J                  ! Axis index
*.

*  Assume no bad values.
      BAD = .FALSE.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round normalising each axis value, and checking for bad values.
      DO I = 1, DIM

*  Copy the position into a local work array.
         DO J = 1, NDIM
            WORK( J ) = POS( I, J )
         END DO

*  Normalise the position.
         CALL AST_NORM( FRM, WORK, STATUS )

*  Store the normalised value on the required axis, indicating if it is
*  bad.
         AXVAL( I ) = WORK( IAXIS )
         IF( WORK( IAXIS ) .EQ. AST__BAD ) BAD = .TRUE.

      END DO

      END
