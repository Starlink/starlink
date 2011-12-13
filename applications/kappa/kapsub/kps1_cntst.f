      SUBROUTINE KPS1_CNTST( IPLOT, IGRP, IPEN, STATUS )
*+
*  Name:
*     KPS1_CNTST

*  Purpose:
*     Establish a pen style from an attribute list in a group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CNTST( IPLOT, IGRP, IPEN, STATUS )

*  Description:
*     This routine sets the plotting style according to a given element
*     in the supplied group.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot.
*     IGRP = INTEGER (Given)
*        A GRP identifier for a group containing descriptions of the pens
*        to be used. Each element in the group should be a comma
*        separated list of AST attribute settings to be established.
*        Any attributes not specified default to their values in the Plot.
*     IPEN = INTEGER (Given)
*        The index withi the group of the style to be established.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     25-JAN-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      INTEGER IPLOT
      INTEGER IGRP
      INTEGER IPEN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER PENDEF*(GRP__SZNAM)! AST attribute settings for current pen
      INTEGER J1                 ! Index at start of attribute setting
      INTEGER J2                 ! Index of comma at end of attribute setting
      LOGICAL BADAT              ! Was attribute setting string invalid?

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the next list of AST Attribute settings from the group.
      CALL GRP_GET( IGRP, IPEN, 1, PENDEF, STATUS )

*  Loop round each comma-delimited attribute in the definitions, translating
*  colour names and any defined synonyms, and storing it in the Plot.
      IF( PENDEF .NE. ' ' ) THEN
         J1 = 1
         DO WHILE( J1 .LE. GRP__SZNAM .AND. STATUS .EQ. SAI__OK )
            J2 = J1
            CALL CHR_FIND( PENDEF, ',', .TRUE., J2 )
            CALL KPG1_ASSTS( PENDEF( J1 : J2 - 1 ), .TRUE.,
     :                       .TRUE., IPLOT, BADAT, STATUS )
            J1 = J2 + 1
         END DO

      END IF

      END
