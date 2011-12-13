      SUBROUTINE SUBPAR_GET ( NAMECODE, STRING, STATUS )
*+
*  Name:
*     SUBPAR_GET

*  Purpose:
*     Get the value of a parameter as a string

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_GET ( NAMECODE, STRING, STATUS )

*  Description:
*     This routine gets the parameter value as a string - as required by
*     the ADAM GET context. If the parameter has not been given a value,
*     an attempt will be made to get one by searching the vpath. The routine
*     should cope with any type of parameter - where a name has been given
*     as the parameter `value' for a primitive type, the name rather than
*     the value in the object will be returned.
*     In the GET context, prompting is disabled - if the vpath requires a
*     prompt, STATUS is returned set to PAR__NOUSR.
*     If the null state is set for the parameter, STATUS is returned set
*     to PAR__NULL.
*     Note also that if the GET context results in a vpath search, DYNAMIC
*     will have no effect.
*     If the parameter state is SUBPAR__MIN or SUBPAR__MAX (ie MIN or MAX
*     has just been set by a SET context message and the task has not yet
*     run to set an actual value) 'MIN' or 'MAX' will be returned.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to the parameter
*     STRING=CHARACTER*(*) (returned)
*        Value to be obtained from the parameter
*     STATUS=INTEGER

*  Algorithm:

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-AUG-1994 (AJC):
*        Original version
*     17-OCT-1994 (AJC):
*        Use GET0C not FETCHC for INTERNALS
*     24-MAY-1995 (AJC):
*        Insert initial STATUS check.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'SUBPAR_PAR'
      INCLUDE 'SUBPAR_PARERR'
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER NAMECODE                  ! Parameter number

*  Arguments Returned:
      CHARACTER * ( * ) STRING          ! Value obtained

*  Status:
      INTEGER STATUS                    ! Global status

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  Local Constants:

*  Local Variables:
      CHARACTER*(DAT__SZLOC) LOC              ! Locator if data stored in HDS
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the parameter type is .GE. 20, a name has been supplied - return the
*  name, preceded by @. This is a change from existing behaviour in the
*  case where an HDS object is supplied as a primitive parameter value.
*  Currently the value in the object would be returned.
      IF ( PARTYPE( NAMECODE ) .GE. 20 ) THEN
         STRING = '@'
         CALL SUBPAR_GETNAME( NAMECODE, STRING(2:), STATUS )

*  Otherwise, Check for MIN or MAX. These will only occur if they have just
*  been SET. After the value has been GOT, the state goes to active and the
*  actual min or max value will be returned. (It may not be worth bothering
*  to allow for the MIN/MAX case.)
      ELSE
         IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__MIN ) THEN
            STRING = 'MIN'

         ELSEIF (PARSTATE(NAMECODE) .EQ. SUBPAR__MAX ) THEN
            STRING = 'MAX'

*  It's not a name, MIN or MAX; Get the parameter value - this will search
*  VPATH etc. except that:
*  1. Dynamic defaults will not be set.
*  2. The GET context does not allow prompting - if it is required to do so,
*     error PAR__NOUSR is returned and errors reported.
         ELSE
*
*      Check whether the parameter is stored internally.
*      If so, get its value as a string.
         IF ( ( PARSTATE(NAMECODE) .NE. SUBPAR__NULL ) .AND.
     :     ( PARVPATH(1,NAMECODE) .EQ. SUBPAR__INTERNAL ) .AND.
     :     ( PARTYPE(NAMECODE) .LT. 10 ) ) THEN
            CALL SUBPAR_GET0C( NAMECODE, STRING, STATUS )
*     Not INTERNAL get locator in the normal way but this will try to open
*     it as a primitive HDS object.
*     This may say it can't open the file
         ELSE
            IF ( PARWRITE(NAMECODE) ) THEN
               CALL SUBPAR_ASSOC ( NAMECODE, 'UPDATE', LOC, STATUS )
            ELSE
               CALL SUBPAR_ASSOC ( NAMECODE, 'READ', LOC, STATUS )
            ENDIF
            CALL DAT_ANNUL( LOC, STATUS )

            IF ( ( STATUS .NE. SAI__OK )
     :      .AND. ( STATUS .NE. PAR__NULL) ) THEN
*        If the ASSOC failed, presumably it was because a name was given
*        which didn't deliver as a primitive HDS object. If type allows,
*        try to get it just as a name.
*        Errors will have been reported and the parameter will have been
*        cancelled so annul errors and set parameter back to ground state.
               IF ( PARTYPE(NAMECODE) .EQ. SUBPAR__NOTYPE ) THEN
                  CALL EMS_ANNUL( STATUS )
                  PARSTATE(NAMECODE) = SUBPAR__GROUND
                  CALL SUBPAR_GETNAME( NAMECODE, STRING, STATUS )
               END IF

            END IF

*        Now get the current value in required format
            CALL SUBPAR_CURVAL( NAMECODE, STRING, STATUS )

         END IF

      END IF


      END IF

      END
