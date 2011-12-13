      SUBROUTINE KPS1_STQA0( IGRP, IC, NDIM, NC, LIST, STATUS )
*+
*  Name:
*     KPS1_STQA0

*  Purpose:
*     Convert text pixel indices into numerical pixel indices.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_STQA0( IGRP, IC, NDIM, NC, LIST, STATUS )

*  Description:
*     This routine attempts to read one integer value from each
*     name stored in the group identified by IGRP, with indices
*     between (IC-1)*NDIM+1 and IC*NDIM (inclusive). The NDIM
*     integers thus obtained are stored in elements (1,IC) to
*     (NDIM,IC) of array LIST.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group holding the indices of a set of
*        pixels. Each string within the group should hold a single
*        index value, and the indices for a single pixel should be in
*        adjacent names (eg in the order X1,Y1,Z1,X2,Y2,Z2... for a
*        three dimensional NDF).
*     IC = INTEGER (Given)
*        The index within the array LIST at which the indices of the
*        pixel are to be stored (i.e. the pixel number).
*     NDIM = INTEGER (Given)
*        The no. of dimensions (e.g. the no. of indices needed to
*        specify the pixel - 2 for a two-dimensional image).
*     NC = INTEGER (Given)
*        The total number of pixels in the group.
*     LIST( NDIM, NC ) = INTEGER (Given and Returned)
*        The list of pixel indices. The indices read from the group are
*        stored in this array starting at LIST(1,IC).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     {enter_new_authors_here}

*  History:
*     19-JUN-1992 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'GRP_PAR'          ! GRP constants.

*  Arguments Given:
      INTEGER IGRP
      INTEGER IC
      INTEGER NDIM
      INTEGER NC

*  Arguments Given and Returned:
      INTEGER LIST( NDIM, NC )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER NAME*(GRP__SZNAM)! Text from the group.
      INTEGER I                  ! Loop count.
      INTEGER START              ! Index of first name.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each name in the group.
      START = ( IC - 1 )*NDIM
      DO I = START + 1, START + NDIM

*  Get the next NAME.
         CALL GRP_GET( IGRP, I, 1, NAME, STATUS )

*  Read the index value from the string.
         CALL CHR_CTOI( NAME, LIST( I - START, IC ), STATUS )

      END DO

      END
