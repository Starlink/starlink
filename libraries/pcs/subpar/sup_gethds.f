      SUBROUTINE SUBPAR_GETHDS ( NAMECODE, STRUCTNAME, ACCESS, LOC,
     :  STATUS )
*+
*  Name:
*     SUBPAR_GETHDS

*  Purpose:
*     Open a named HDS object associated with a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_GETHDS ( NAMECODE, STRUCTNAME, ACCESS, LOC,

*  Description:
*     Given the index of a program parameter, and the name of an HDS
*     structure, an attempt is made to access the structure. If this is
*     successful and the object is of a suitable type, the information
*     associated with the parameter is stored and the HDS locator
*     is returned.
*     If the routine fails, the locator is nullified (set to blanks).

*  Arguments:
*     NAMECODE=INTEGER (given)
*        Internal number identifying program parameter
*     STRUCTNAME=CHARACTER*(*) (given)
*        Name of HDS structure
*     ACCESS=CHARACTER*(*) (given)
*        Access mode, 'READ', 'WRITE' or 'UPDATE'
*     LOC=CHARACTER*DAT__SZLOC (returned)
*        Locator to data structure
*     STATUS=INTEGER
*        Status return

*  Algorithm:
*     STRUCTNAME is interpreted as a VMS filename (an HDS container
*     file), followed by the full name of the structure component
*     required. A locator is requested for the named component.
*     If this is successful, the parameter is marked as active and as
*     stored externally, and the structure name is copied into the
*     parameter's internal character store.

*  Copyright:
*     Copyright (C) 1984, 1988, 1989, 1991, 1993 Science & Engineering Research Council.
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
*     25-SEP-1984 (BDK):
*        Original
*     16-AUG-1988 (AJC):
*        Include SUBPAR_PAR - How did it work before
*     14-DEC-1988 (AJC):
*        Put the name in tables if located. At end cancel if
*        not OK; otherwise save current name externally
*     02-FEB-1989 (AJC):
*        guard against hanging locators
*     30-JUL-1991 (AJC):
*        remove unused VMS references
*        add EMS error reporting
*     24-SEP-1991 (AJC):
*        prefix messages with 'SUBPAR:'
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

      CHARACTER*(*) STRUCTNAME     ! character string associated
                                   ! with the parameter

      CHARACTER*(*) ACCESS         ! Access mode, 'READ', 'WRITE'
                                   ! or 'UPDATE'


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
      CHARACTER*(DAT__SZLOC) FILOC         ! HDS locator to top-level of
                                           ! the structure
      CHARACTER*(DAT__SZLOC) BOTLOC        ! HDS locator (temporary)


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise output locator
      LOC = ' '
*
*   Get the HDS locators to the top and bottom level objects for the
*   (named data structure.
*
      CALL SUBPAR_HDSLOCS ( STRUCTNAME, ACCESS, FILOC, BOTLOC, STATUS )
*
*   Store the name in the parameter tables. Will not if bad status.
      CALL SUBPAR_PUTNAME ( NAMECODE, STRUCTNAME, STATUS )
*
*   Store internally the topmost-level locator ( so that the file
*   can be closed when necessary ), and the bottom-level locator.
*   Take a copy of the bottom-level locator, which is to be
*   returned to the calling routine.
*
      CALL SUBPAR_PUTFLOC ( NAMECODE, FILOC, STATUS )
      CALL SUBPAR_PUTLOC ( NAMECODE, BOTLOC, STATUS )
      CALL DAT_CLONE ( BOTLOC, LOC, STATUS )

*
*   Associate the parameter name with both copies of the
*   bottom-level locator within HDS so that DAT_CANCL can be used
*   to annul the locator without knowing its value.
*   -- NB this must not be done with FILOC - it must only be
*   annulled via a call to HDS_CLOSE.
*
      CALL HDS_LINK ( BOTLOC, SUBPAR_PARGP(NAMECODE), STATUS )
      CALL HDS_LINK ( LOC, SUBPAR_PARGP(NAMECODE), STATUS )
*
*   If OK, update the external current value
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL SUBPAR_CURSAV ( NAMECODE, STRUCTNAME, STATUS )
      ENDIF

*   If the routine failed, cancel the parameter and report
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL SUBPAR_CANCL ( NAMECODE, STATUS )
         CALL EMS_SETC ( 'NAME', PARKEY(NAMECODE) )
         CALL EMS_REP ( 'SUP_GETHDS1',
     :   'SUBPAR: Failed to open HDS object associated with '//
     :   'parameter ^NAME', STATUS )
      ENDIF

      END
