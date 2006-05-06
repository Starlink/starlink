      SUBROUTINE CCDPACK_RED( STATUS )
*+
*  Name:
*     CCDPACK_RED

*  Purpose:
*     Top-level monolith for reduction tasks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCDPACK( STATUS )

*  Description:
*     This routine gets the action name directly from the Unix kernel.
*     It then calls the appropriate routine to perform the
*     specified action. An error will be reported and STATUS will
*     be set if the action name is not recognized.

*  Arguments:
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-OCT-1995 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! Parameter system constants
      INCLUDE 'CCD1_PAR'         ! VERS value.

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( PAR__SZNAM ) ACTION ! Action name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( ACTION, STATUS )

*  Set the NDF application name.
      CALL NDF_HAPPN( ACTION // VERS, STATUS )

*  Tweak the numerics on a RedHat 7 Linux system.
      CALL CCD1_LINFLT

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...  
      IF ( ACTION .EQ. 'CALCOR' ) THEN
         CALL CALCOR( STATUS )

      ELSE IF ( ACTION .EQ. 'DEBIAS' ) THEN
         CALL DEBIAS( STATUS )

      ELSE IF ( ACTION .EQ. 'FLATCOR' ) THEN
         CALL FLATCOR( STATUS )

      ELSE IF ( ACTION .EQ. 'MAKEBIAS' ) THEN
         CALL MAKEBIAS( STATUS )

      ELSE IF ( ACTION .EQ. 'MAKECAL' ) THEN
         CALL MAKECAL( STATUS )

      ELSE IF ( ACTION .EQ. 'MAKEFLAT' ) THEN
         CALL MAKEFLAT( STATUS )

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ACTION', ACTION )
         CALL ERR_REP( 'CCDPACK_ERR',
     :                 'CCDPACK: The action name ''^ACTION'' is ' //
     :                 'not recognised by the CCDPACK '//
     :                 'monolith.', STATUS )
      END IF

      END
* $Id$
