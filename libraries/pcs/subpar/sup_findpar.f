      SUBROUTINE SUBPAR_FINDPAR ( NAME, NAMECODE, STATUS )
*+
*  Name:
*     SUBPAR_FINDPAR

*  Purpose:
*     Search parameter-list for named parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_FINDPAR ( NAME, NAMECODE, STATUS )

*  Description:
*     Search the list of parameters for the given name, and if it is found,
*     return its index.

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*        name of requested parameter
*     NAMECODE=INTEGER (returned)
*        index number of parameter, if found.
*     STATUS=INTEGER

*  Algorithm:
*     The list of parameter names is searched sequentially over the
*     range relevant to the current program (if within a monolith).

*  Copyright:
*     Copyright (C) 1984, 1985, 1991, 1993 Science & Engineering Research Council.
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
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02-OCT-1984 (BDK):
*        Original
*     23-AUG-1985 (BDK):
*        handle monoliths
*     16-JUL-1991 (AJC):
*        use CHR not STR$ for portability
*     17-OCT-1991 (AJC):
*        add error report and change PARSE__NOPAR to SUBPAR__NOPAR
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
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
      LOGICAL FOUND


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      NAMECODE = PROGADD(1,PROGNUM) - 1
      FOUND = .FALSE.
*
*   Force given name to uppercase
*
      INNAME = NAME
      CALL CHR_UCASE ( INNAME )
*
*   Search the list of declared names
*
      DO WHILE ( ( .NOT. FOUND ) .AND. ( NAMECODE .LT.
     :  PROGADD(2,PROGNUM) ) )

         NAMECODE = NAMECODE + 1

         IF ( INNAME .EQ. PARNAMES(NAMECODE) ) FOUND = .TRUE.

      ENDDO

      IF ( .NOT. FOUND ) THEN
         STATUS = SUBPAR__NOPAR
         CALL EMS_SETC ( 'PARAM', INNAME )
         CALL EMS_REP ( 'SUP_FINDPAR1',
     :   'SUBPAR: Parameter ^PARAM not defined in interface file',
     :    STATUS )
         NAMECODE = 0
      ENDIF

      END
