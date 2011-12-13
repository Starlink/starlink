      SUBROUTINE KPS1_MSVP( IGRP, NNDF, VAR, STATUS )
*+
*  Name:
*     KPS1_MSVP

*  Purpose:
*     Determines the presence of VARIANCE components in a group
*     of NDFs for MSTATS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MSVP( IGRP, NNDF, VAR,STATUS )

*  Description:
*     This routine tests a supplied group of NDFs for the presence of a
*     VARIANCE component in all of them.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The GRP identifier of a group of NDFs to be examined.
*     NNDF = INTEGER (Given)
*        The number of NDFs in the group (size of IGRP).
*     VAR = LOGICAL (Returned)
*        This is set .TRUE. if all the NDFs within the supplied group
*        have VARIANCE components, and .FALSE. otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     2008 May 5 (MJC):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER IGRP
      INTEGER NNDF

*  Arguments Returned:
      LOGICAL VAR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER NDFI               ! NDF identifier
      LOGICAL NDFVAR             ! VARIANCE present in a single NDF?

*.

      VAR = .FALSE.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Try to open all the NDFs.
      DO I = 1, NNDF

*  Open an NDF.
         CALL NDG_NDFAS( IGRP, I, 'READ', NDFI, STATUS )

*  See if the input NDF has a VARIANCE component.
         CALL NDF_STATE( NDFI, 'VARIANCE', NDFVAR, STATUS )

*  Break from the loop when the answer is known.
         IF ( .NOT. NDFVAR ) GOTO 999
      END DO
      VAR = .TRUE.

*  Error exit label.
  999 CONTINUE

      END
