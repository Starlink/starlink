      SUBROUTINE NDG_CPSUP( IGRP1, I, IGRP2, STATUS )
*+
*  Name:
*     NDG_CPSUP

*  Purpose:
*     Copy supplemental information for an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_CPSUP( IGRP1, I, IGRP2, STATUS )

*  Description:
*     Copies an entry with its supplemental information from one group to
*     another, appending it to the end of the output group.

*  Arguments:
*     IGRP1 = INTEGER (Given)
*        The NDG group as returned by NDG_ASSOC, etc.
*     I = INTEGER (Given)
*        The index, within IGRP1, of the entry to copy.
*     IGRP2 = INTEGER (Given)
*        The NDG group to which the copied information should be appended.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-MAR-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.

*  Arguments Given:
      INTEGER IGRP1
      INTEGER I
      INTEGER IGRP2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER FIELDS( 6 )*(GRP__SZNAM)
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the information to copy from IRGP1.
      CALL NDG_GTSUP( IGRP1, I, FIELDS, STATUS )

*  Append it to the end of the second group.
      CALL NDG_PTSUP( IGRP2, 0, FIELDS, STATUS )

      END
