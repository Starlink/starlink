      SUBROUTINE SUBPAR_CURNAME ( NAMECODE, STRING, STATUS )
*+
*  Name:
*     SUBPAR_CURNAME

*  Purpose:
*     Get the current value of a parameter if a name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_CURNAME ( NAMECODE, STRING, STATUS )

*  Description:
*     Given the index of a program parameter, try to get a locator to
*     its associated 'private' HDS storage. If this is successful, check
*     that an HDS name is stored there, and get the name.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        Internal number identifying program parameter
*     STRING=CHARACTER*(*) (returned)
*        value of parameter as a string
*     STATUS=INTEGER
*        Status return

*  Algorithm:
*     The top-level locator to 'private' storage is obtained from
*     COMMON. A locator is requested for the named component.
*     If this is successful, if the value is a structure name return it.

*  Copyright:
*     Copyright (C) 1987, 1990, 1992, 1993 Science & Engineering Research Council.
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
*     18-AUG-1987 (BDK):
*        Original
*     05-FEB-1990 (AJC):
*        Guard against hanging locators
*     10-NOV-1992 (AJC):
*        Use SUBPAR__ERROR not PAR__
*        and report error
*     11-FEB-1993 (AJC):
*        Correct EMS calls
*     26-FEB-1993 (AJC):
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
      INTEGER NAMECODE             ! Number of program parameter


*  Arguments Returned:
      CHARACTER*(*) STRING         ! value of parameter as a string


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      CHARACTER*(DAT__SZLOC) BOTLOC        ! HDS locator (temporary)

      CHARACTER*(DAT__SZLOC) LOC   ! Locator to data structure

      CHARACTER*15 HDSTYPE         ! type of named object

      LOGICAL PRIM                 ! .TRUE. => primitive object

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Ask for a component in 'private' storage with the same name as the
*   parameter.
*
      BOTLOC = ' '
      CALL DAT_FIND ( EXTLOC, PARNAMES(NAMECODE), BOTLOC, STATUS )

*
*   Find what kind of an object has been located.
*   If it is a structure, then it is the structure name required.
*
      CALL DAT_TYPE ( BOTLOC, HDSTYPE, STATUS )
      CALL DAT_PRIM ( BOTLOC, PRIM, STATUS )

      IF (( STATUS .EQ. SAI__OK) .AND. ( .NOT. PRIM )) THEN
*
*      The value is a structure. If it is a pointer to
*      another structure, find the real structure name required.
*
         IF ( HDSTYPE .EQ. 'ADAM_PARNAME' ) THEN

            LOC = ' '
            CALL DAT_FIND ( BOTLOC, 'NAMEPTR', LOC, STATUS )
            CALL DAT_GETC ( LOC, 0, 0, STRING, STATUS )
            CALL DAT_ANNUL ( LOC, STATUS )

         ELSE

*        Impossible type - force continuation
            STATUS = SUBPAR__ERROR
            CALL EMS_SETC( 'PARAM', PARNAMES(NAMECODE) )
            CALL EMS_REP ( 'SUP_CURNAME1',
     :      'SUBPAR_CURNAME: Parameter ^PARAM - ' //
     :      'Illegal parameter file object', STATUS )

         ENDIF

      ELSE

*     Current value is not a name - force continuation
         STATUS = SUBPAR__ERROR
            CALL EMS_SETC( 'PARAM', PARNAMES(NAMECODE) )
            CALL EMS_REP ( 'SUP_CURNAME2',
     :      'SUBPAR_CURNAME: Parameter ^PARAM - ' //
     :      'Current value is not a name', STATUS )

      ENDIF

      CALL DAT_ANNUL ( BOTLOC, STATUS )

      END
