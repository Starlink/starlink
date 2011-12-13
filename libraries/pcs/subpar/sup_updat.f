      SUBROUTINE SUBPAR_UPDAT ( NAMECODE, STATUS )
*+
*  Name:
*     SUBPAR_UPDAT

*  Purpose:
*     Force HDS update.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_UPDAT ( NAMECODE, STATUS )

*  Description:
*     Force the HDS file associated with the indicated parameter to be
*     updated, so that its memory cache coincides with the data on disk.

*  Arguments:
*     NAMECODE=INTEGER
*        pointer to parameter associated with an HDS structure
*     STATUS=INTEGER

*  Algorithm:
*     Use the locator to the container file for the parameter in a call
*     to HDS_FREE.

*  Implementation Deficiencies:
*     Not part of the SSE interfaces. This routine is provided
*     specifically for ADAM multitasking operations.

*  Copyright:
*     Copyright (C) 1985, 1993 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     18-APR-1985 (BDK):
*        Original
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
      INTEGER NAMECODE         ! pointer to parameter associated with an
                               ! HDS structure


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      CHARACTER*(DAT__SZLOC) FILOC   ! top-level locator to HDS file


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Check the given parameter has locators associated with it.
*   If it doesn't, do nothing.
*
      IF ( PARVALID(NAMECODE) ) THEN

         FILOC = PARLOC(1,NAMECODE)
         CALL HDS_FREE ( FILOC, STATUS )

      ENDIF

      END
