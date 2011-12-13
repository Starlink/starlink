      SUBROUTINE SUBPAR_FINDACT ( NAME, NAMECODE, STATUS )
*+
*  Name:
*     SUBPAR_FINDACT

*  Purpose:
*     Search action-list keywords for named action.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_FINDACT ( NAME, NAMECODE, STATUS )

*  Description:
*     Search the list of action keywords for the given name, and if it
*     is found, return its index. For monoliths also initialize the top
*     locator for the HDS parameter store and set the program number.

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*        keyword of requested action
*     NAMECODE=INTEGER (returned)
*        index number of action, if found.
*     STATUS=INTEGER

*  Algorithm:
*     Force the given name to uppercase.
*     The list of action keywords is searched sequentially. If the required
*     action is found, its index is returned in NAMECODE; if it is not found
*     an error is reported unless there is only one action, in which case
*     NAMECODE is returned as 1
*
*     For monoliths, set up EXTLOC, DYNLOC and PROGNUM. (Set PROGNUM=0 if
*     the action name was not found.)

*  Copyright:
*     Copyright (C) 1984, 1985, 1986, 1987, 1989, 1991, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1996, 2000 Central Laboratory of the Research Councils.
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
*     BDK: B D Kelly (ROE)
*     KS: K Shortridege (AAO)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02-OCT-1984 (BDK):
*        Original
*     23-AUG-1985 (BDK):
*        handle monoliths - set up EXTLOC
*     28-AUG-1985 (BDK):
*        don't set PROGNUM and EXTLOC for non-monoliths
*     06-SEP-1985 (BDK):
*        set up DYNLOC for monoliths
*     17-MAR-1986 (BDK):
*        allow Atasks ANY action name
*     26-MAY-1987 (BDK):
*        search ACTKEYs rather than ACTNAMES
*     01-MAR-1989 (KS):
*        HDS monolith code moved from SUBPAR_ACTIV to this
*        routine so that .SDF entries only created as needed
*        rather than at startup.
*     16-JUL-1991 (AJC):
*        Use CHR not STR$ for portability
*     22-JAN-1992 (AJC):
*        set SUBPAR not PARSE error
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     10-OCT-1996 (AJC):
*        Link locators to group PROGRAM
*     15-FEB-2000 (AJC):
*        Set PROGNUM to NAMECODE for monoliths even if
*        NAMECODE=0 because action was not found
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_ERR'


*  Arguments Given:
      CHARACTER*(*) NAME                 ! name to be found


*  Arguments Returned:
      INTEGER NAMECODE                   ! index of NAME if found


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      CHARACTER*15 INNAME         ! name forced to uppercase
      LOGICAL THERE
      LOGICAL FOUND
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      NAMECODE = 0
      FOUND = .FALSE.
*
*   Force given name to uppercase
*
      INNAME = NAME
      CALL CHR_UCASE ( INNAME )
*
*   Search the list of declared names
*
      DO WHILE ( ( .NOT. FOUND ) .AND. ( NAMECODE .LT. ACTPTR ) )

         NAMECODE = NAMECODE + 1
         IF ( INNAME .EQ. ACTKEY(NAMECODE) ) FOUND = .TRUE.

      ENDDO

      IF ( .NOT. FOUND ) THEN
*   If there is only one action for a non-monolith we assume that is
*   the one required - regardless of the name given
         IF ( .NOT. MONOLITH .AND. ( ACTPTR .EQ. 1 ) ) THEN
            NAMECODE = 1
         ELSE
*   Otherwise we report an error and set NAMECODE = 0
            STATUS = SUBPAR__NOACT
            CALL EMS_SETC ( 'ACT', INNAME )
            CALL EMS_REP ( 'SUB_FINDACT1',
     :      'SUBPAR: Action ^ACT is not defined', STATUS )
            NAMECODE = 0
         ENDIF
      ENDIF

*
*   For a monolith, set the program number (which may be zero if the action
*   name wasn't found) and, if all is OK, find the locator to this program
*   ( =action ) and the store for dynamic defaults.
      IF ( MONOLITH ) THEN

         PROGNUM = NAMECODE

         IF ( STATUS .EQ. SAI__OK ) THEN

            CALL DAT_THERE ( EXTTOP, ACTNAMES(NAMECODE),
     :                                               THERE, STATUS )
            IF ( .NOT. THERE ) THEN
               CALL DAT_NEW ( EXTTOP, ACTNAMES(NAMECODE),
     :                                       'PROGRAM', 0, 0, STATUS )
               CALL DAT_FIND ( EXTTOP, ACTNAMES(NAMECODE), EXTLOC,
     :                                                        STATUS )
               CALL DAT_NEW ( EXTLOC, 'ADAM_DYNDEF', 'DEFAULTS', 0,
     :              0, STATUS )
            ELSE
               CALL DAT_FIND ( EXTTOP, ACTNAMES(NAMECODE), EXTLOC,
     :                                                         STATUS )
            END IF
            CALL DAT_FIND ( EXTLOC, 'ADAM_DYNDEF', DYNLOC, STATUS )

*         Link locators to PROGRAM group
            CALL HDS_LINK( EXTLOC, 'PROGRAM', STATUS )
            CALL HDS_LINK( DYNLOC, 'PROGRAM', STATUS )

         ENDIF

      ENDIF

      END
