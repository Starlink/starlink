      SUBROUTINE SST_MON( STATUS )
*+
*  Name:
*    SST_MON

*  Purpose:
*     Top-level ADAM monolith routine for the SST_MON package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_MON( STATUS )

*  Description:
*     This routine obtains the name of the current action and calls the
*     appropriate routine to perform the specified operation. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     5-DEC-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! PAR_ public constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( PAR__SZNAM ) NAME ! Action name

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the action name.
      CALL TASK_GET_NAME( NAME, STATUS )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...

      IF ( NAME .EQ. 'FORSTATS' ) THEN
         CALL FORSTATS( STATUS )

      ELSE IF ( NAME .EQ. 'PROCVT' ) THEN
         CALL PROCVT( STATUS )

      ELSE IF ( NAME .EQ. 'PROHLP' ) THEN
         CALL PROHLP( STATUS )

      ELSE IF ( NAME .EQ. 'PROLAT' ) THEN
         CALL PROLAT( STATUS )

      ELSE IF ( NAME .EQ. 'PROPAK' ) THEN
         CALL PROPAK( STATUS )

      ELSE IF ( NAME .EQ. 'PROHTML' ) THEN
         CALL PROHTML( STATUS )

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NAME', NAME )
         CALL ERR_REP( 'SST_MON_ERR',
     :        'SST_MON: The action name ''^NAME'' is ' //
     :        'not recognised by the SST_MON monolith.',
     :        STATUS )
      END IF

      END
* @(#)sst_mon.f   1.2   94/12/06 11:14:33   96/07/05 10:27:41
