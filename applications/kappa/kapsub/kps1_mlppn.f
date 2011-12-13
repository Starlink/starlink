      SUBROUTINE KPS1_MLPPN( PARAM, IGRP, STATUS )
*+
*  Name:
*     KPS1_MLPPN

*  Purpose:
*     Create pen definitons with which to draw each curve.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MLPPN( PARAM, IGRP, STATUS )

*  Description:
*     This routine returns a group holding AST attribute settings which
*     define the appearance required for each curve drawn by MLINPLOT.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter to use to get the user-specified pens for each
*        curve (should be 'PENS').
*     IGRP = INTEGER (Returned)
*        A GRP identifier for a group holding the AST attribute settings
*        defining each pen. The group contains one element for each
*        curve. Each element holds a comma separated list of
*        attribute settings for an AST Plot.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     {enter_new_authors_here}

*  History:
*     12-AUG-1999 (DSB):
*        Original version.
*     2006 April 12 (MJC):
*        Remove unused variables and wrapped long lines.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Returned:
      INTEGER IGRP

*  Status:
      INTEGER STATUS              ! Global status

*  Local Variables:
      INTEGER NPEN                ! No. of explicit pen definitions given
*.

*  Initialise.
      IGRP = GRP__NOID

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a new group to contain the pen definitions.
      CALL GRP_NEW( 'Pen definitions', IGRP, STATUS )

*  Use a semicolon as the separator character because AST required
*  the individual attribute settings within the pen definition to be
*  separated by commas.
      CALL GRP_SETCC( IGRP, 'DELIMITER', ';', STATUS )

*  Get a group of strings holding pen definitions from the environment.
      CALL KPG1_GTGRP( PARAM, IGRP, NPEN, STATUS )

*  Annul the error if a null value was given.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NPEN = 0
      END IF

*  Delete the group holding pen definitions if an error has occurred, or
*  if no pen defintions were supplied.
      IF( IGRP .NE. GRP__NOID ) THEN
         IF( NPEN .EQ. 0 .OR. STATUS .NE. SAI__OK ) THEN
            CALL GRP_DELET( IGRP, STATUS )
         END IF
      END IF

      END
