      SUBROUTINE KPG1_ARCOG( PARAM, INDF, MCOMP, COMP, STATUS )
*+
*  Name:
*     KPG1_ARCOG

*  Purpose:
*     Allows the user to select an array component in a supplied NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ARCOG( PARAM, INDF, MCOMP, COMP, STATUS )

*  Description:
*     This routine allows the user to select an NDF array component
*     selected from the ones available in the supplied NDF. Any of
*     'Data', 'Quality', 'Variance', and 'Error' can be selected if
*     they are present in the NDF. The user is re-prompted until a
*     valid component name is obtained, or an error occurs.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter to use.
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     MCOMP = CHARACTER * ( * ) (Returned)
*        The name of the selected array component for use with NDF_MAP.
*        The  returned values are 'Data', 'Quality', 'Variance', and
*        'Error' for the array components DATA, QUALITY, VARIANCE, and
*        ERROR respectively. Returned equal to 'Data' if an error occurs.
*     COMP = CHARACTER * ( * ) (Returned)
*        Equal to MCOMP except that 'Variance' is substituted in place of
*        'Error'. Only NDF_MAP accepts 'Error' as a component name. All
*        other NDF routines require 'Variance' to be specified instead
*        and so should use COMP instead of MCOMP.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-MAR-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER INDF

*  Arguments Returned:
      CHARACTER MCOMP*(*)
      CHARACTER COMP*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL LOOP               ! Is a new parameter value required?
      LOGICAL THERE              ! Does component exist?
*.

*  Initialise.
      MCOMP = 'Data'
      COMP = 'Data'

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop until a valid component name has been obtained, or an error occurs.
      LOOP = .TRUE.
      DO WHILE( LOOP .AND. STATUS .EQ. SAI__OK )

*  Obtain a component name, chosen from all possible component names.
         CALL PAR_CHOIC( PARAM, 'Data', 'Data,Quality,Error,Variance',
     :                   .FALSE., MCOMP, STATUS )


*  PAR_CHOIC returns strings in upper case. Convert all but the first
*  character to lower cawe.
        CALL CHR_LCASE( MCOMP( 2: ) )

*  Most NDF routines with a component argument don't recognise
*  'Error', so we need two variables.  Thus convert 'Error' into
*  'Variance' in the variable needed for such routines.  The original
*  value is held in a variable with the prefix M for mapping, as one
*  of the few routines that does support 'Error' is NDF_MAP.
         IF( MCOMP .NE. 'Error' ) THEN
            COMP = MCOMP
         ELSE
            COMP = 'Variance'
         END IF

*  See if the selected component is in a defined state.
         CALL NDF_STATE( INDF, COMP, THERE, STATUS )

*  If the selected component is not available, and no error has occurred...
         IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN

*  Report an error.
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'MCOMP', MCOMP )
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'KPG1_ARCOG_1',  'There is no ^MCOMP '//
     :                    'component in ^NDF.', STATUS )

*  Flush the error.
            CALL ERR_FLUSH( STATUS )

*  Cancel the parameter value.
            CALL PAR_CANCL( PARAM, STATUS )

*  If a usable component name was obtained, or an error occurred, quit
*  looping.
         ELSE
            LOOP = .FALSE.
         END IF

      END DO

*  Return 'Data' if an error has occurred.
      IF( STATUS .NE. SAI__OK ) THEN
         MCOMP = 'Data'
         COMP = 'Data'
      END IF

      END
