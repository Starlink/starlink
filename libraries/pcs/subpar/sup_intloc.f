      SUBROUTINE SUBPAR_INTLOC ( NAMECODE, LOC, STATUS )
*+
*  Name:
*     SUBPAR_INTLOC

*  Purpose:
*     Get locator to 'private' store associated with a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_INTLOC ( NAMECODE, LOC, STATUS )

*  Description:
*     Given the index of a program parameter, try to get a locator to
*     its associated 'private' HDS storage. If this is
*     successful, the information is stored associated with the
*     parameter and the HDS locator is returned.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        Internal number identifying program parameter
*     LOC=CHARACTER*DAT__SZLOC (returned)
*        Locator to data structure
*     STATUS=INTEGER
*        Status return

*  Algorithm:
*     The top-level locator to 'private' storage is obtained from
*     COMMON. A locator is requested for the named component.
*     If this is successful, the parameter is marked as active and as
*     stored 'privately', and the locators are copied into the
*     parameter's internal character store. A clone of the bottom-level
*     locator is returned.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
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
*     01-OCT-198 (BDK):
*        Original
*     16-AUG-198 (AJC):
*        Include SUBPAR_PAR - How did it work before
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      9-AUG-1993 (AJC):
*        Remove INCLUDE PAR_ERR
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
      INCLUDE 'SUBPAR_PAR'


*  Arguments Given:
      INTEGER NAMECODE             ! Number of program parameter


*  Arguments Returned:
      CHARACTER*(DAT__SZLOC) LOC   ! Locator to data structure


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  External Functions:
      CHARACTER*(DAT__SZGRP) SUBPAR_PARGP           ! HDS group name
      EXTERNAL SUBPAR_PARGP


*  Local Variables:
      CHARACTER*(DAT__SZLOC) BOTLOC        ! HDS locator (temporary)


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Ask for a component in 'private' storage with the same name as the
*   parameter.
*
      CALL DAT_FIND ( EXTLOC, PARNAMES(NAMECODE), BOTLOC, STATUS )
*
*   Store the locators with the parameter, and link them to the
*   parameter's name
*   Take a copy of the bottom-level locator, which is to be
*   returned to the calling routine.
*
      CALL SUBPAR_PUTFLOC ( NAMECODE, EXTLOC, STATUS )
      CALL SUBPAR_PUTLOC ( NAMECODE, BOTLOC, STATUS )
      CALL DAT_CLONE ( BOTLOC, LOC, STATUS )
*
*    Associate the parameter name with both copies of the
*    bottom-level locator within HDS so that DAT_CANCL can be used
*    to annul the locator without knowing its value.
*    -- NB this must not be done with FILOC - it must only be
*    annulled via a call to HDS_CLOSE.
*
      CALL HDS_LINK ( BOTLOC, SUBPAR_PARGP(NAMECODE), STATUS )
      CALL HDS_LINK ( LOC, SUBPAR_PARGP(NAMECODE), STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
*
*      Store the information for the parameter
*
         PARSTATE(NAMECODE) = SUBPAR__ACTIVE
         PARTYPE(NAMECODE) = 10 + MOD ( PARTYPE(NAMECODE), 10 )

      ENDIF

      END
