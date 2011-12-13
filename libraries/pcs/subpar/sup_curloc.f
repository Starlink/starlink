      SUBROUTINE SUBPAR_CURLOC ( NAMECODE, ACCESS, LOC, STATUS )
*+
*  Name:
*     SUBPAR_CURLOC

*  Purpose:
*     Gets locator for current parameter value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_CURLOC ( NAMECODE, ACCESS, LOC, STATUS )

*  Description:
*     Gets an HDS locator to the 'current (ie last) value for a parameter.
*     If the routine fails, the locator will be nullified, ie set to
*     blank.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        number of parameter
*     ACCESS=CHARACTER*(*) (given)
*        required access mode
*     LOC=CHARACTER*(DAT__SZLOC) (returned)
*        HDS locator to the value
*     STATUS=INTEGER

*  Algorithm:
*     Look in the task's private storage. If it contains an actual
*     value, return a locator to it. If it is an HDS name, get a locator
*     to the named structure and return that.

*  Copyright:
*     Copyright (C) 1987, 1990, 1991, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     22-JUL-1991 (AJC):
*        Remove unused declaration THERE
*     10-NOV-1992 (AJC):
*        Use SUBPAR__ERROR not PAR__ERROR
*        report error
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      3-FEB-2000 (AJC):
*        Use SUBPAR_PARGP to get an HDS group name for the parameter
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
      INCLUDE 'SUBPAR_PAR'


*  Arguments Given:
      INTEGER NAMECODE                ! parameter number

      CHARACTER*(*) ACCESS            ! read, write or update


*  Arguments Returned:
      CHARACTER*(DAT__SZLOC) LOC      ! HDS locator to value


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  External Functions:
      CHARACTER*(DAT__SZGRP) SUBPAR_PARGP           ! HDS group name
      EXTERNAL SUBPAR_PARGP


*  Local Variables:
      CHARACTER*(DAT__SZLOC) FILOC   ! top-level locator to file for
                                     ! actual value

      CHARACTER*(DAT__SZLOC) BOTLOC  ! locator to element in program
                                     ! 'private' file

      CHARACTER*15 HDSTYPE

      LOGICAL PRIM


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Ask for a component in 'private' storage with the same name as the
*   parameter.
*
      BOTLOC = ' '
      CALL DAT_FIND ( EXTLOC, PARNAMES(NAMECODE), BOTLOC, STATUS )
*
*   Find what kind of an object has been located. If it is a primitive,
*   then it is the one required. If it is a structure, then it is
*   a pointer to another structure.
*
      CALL DAT_TYPE ( BOTLOC, HDSTYPE, STATUS )
      CALL DAT_PRIM ( BOTLOC, PRIM, STATUS )

      IF (( STATUS.EQ.SAI__OK ) .AND. ( PRIM )) THEN
*
*      A primitive object. Store the locators with the parameter and
*      link them to the parameter's name.
*      Take a copy of the bottom-level locator which is to be returned
*      to the calling routine.
*
         CALL SUBPAR_PUTFLOC ( NAMECODE, EXTLOC, STATUS )
         CALL SUBPAR_PUTLOC ( NAMECODE, BOTLOC, STATUS )
         CALL DAT_CLONE ( BOTLOC, LOC, STATUS )
         CALL HDS_LINK ( LOC, SUBPAR_PARGP(NAMECODE), STATUS )
         CALL HDS_LINK ( BOTLOC, SUBPAR_PARGP(NAMECODE), STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN
            PARSTATE(NAMECODE) = SUBPAR__ACTIVE
            PARTYPE(NAMECODE) = 10 + MOD ( PARTYPE(NAMECODE), 10 )
         ENDIF

      ELSEIF ( STATUS .EQ. SAI__OK ) THEN
*
*      The 'current' value is a structure. If it is a pointer to
*      another structure, find the real structure name required.
*
         IF ( HDSTYPE .EQ. 'ADAM_PARNAME' ) THEN

            LOC = ' '
            CALL DAT_FIND ( BOTLOC, 'NAMEPTR', LOC, STATUS )
            CALL DAT_GETC ( LOC, 0, 0, PARVALS(NAMECODE), STATUS )
            CALL DAT_ANNUL ( LOC, STATUS )
            CALL DAT_ANNUL ( BOTLOC, STATUS )
*         _HDSLOCS will nullify the locators if it fails
            CALL SUBPAR_HDSLOCS ( PARVALS(NAMECODE), ACCESS, FILOC,
     :        BOTLOC, STATUS )

            CALL DAT_CLONE ( BOTLOC, LOC, STATUS )
            CALL SUBPAR_PUTFLOC ( NAMECODE, FILOC, STATUS )
            CALL SUBPAR_PUTLOC ( NAMECODE,  BOTLOC, STATUS )
            CALL HDS_LINK ( LOC, SUBPAR_PARGP(NAMECODE), STATUS )
            CALL HDS_LINK ( BOTLOC, SUBPAR_PARGP(NAMECODE), STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN
               PARSTATE(NAMECODE) = SUBPAR__ACTIVE
               PARTYPE(NAMECODE) = 20 + MOD ( PARTYPE(NAMECODE), 10 )
            ENDIF

         ELSE

*        Object is impossible type - force continuation
            STATUS = SUBPAR__ERROR
            CALL EMS_SETC( 'PARAM', PARNAMES(NAMECODE) )
            CALL EMS_REP ( 'SUP_CURLOC1',
     :      'SUBPAR_CURLOC: Parameter ^PARAM - ' //
     :      'Illegal parameter file object', STATUS )

         ENDIF

      ENDIF

      END
