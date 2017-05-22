      SUBROUTINE SUBPAR_CANCL ( NAMECODE, STATUS )
*+
*  Name:
*     SUBPAR_CANCL

*  Purpose:
*     cancel association between a parameter and a data object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_CANCL ( NAMECODE, STATUS )

*  Description:
*     An existing association between the parameter and a data
*     system object is cancelled, and the container file for it closed.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        Index number of program parameter
*     STATUS=INTEGER

*  Algorithm:
*     The locators associated with the parameter are obtained from
*     internal storage, and if they are valid, all locators associated
*     with the parameter name are flushed. Unless the parameter is
*     associated with the temporary HDS parameter store, the HDS
*     container file is closed.
*     The routine operates even if the entry status is not OK.

*  Implementation Deficiencies:
*     (1) If the parameter does not have a data structure associated with
*     it, then this is not considered an error condition. This is how
*     DAT_CANCL operates in SSE 0.75.
*     (2) The SSE DAT_CANCL does not close HDS container files.

*  Copyright:
*     Copyright (C) 1984, 1988, 1993 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     20-AUG-1984 (BDK):
*        Original
*     16-AUG-1988 (AJC):
*        Include SUBPAR_PAR - How did it work before
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      3-FRB-2000 (AJC):
*        Use SUBPAR_PARGP to get the HDS group name for the parameter
*     22-MAY-2017 (DSB):
*        Use an error reporting environment instead of just saving the
*        status value. The old code causes any pre-existing error
*        messages to be lost if emsAnnul was called within (or below)
*        this routine.
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
      INCLUDE 'SUBPAR_ERR'


*  Arguments Given:
      INTEGER NAMECODE               ! pointer to internal parameter
                                     ! storage


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  External Functions:
      CHARACTER*(DAT__SZGRP) SUBPAR_PARGP
      EXTERNAL SUBPAR_PARGP


*  Local Variables:
      CHARACTER*(DAT__SZLOC) FILOC   ! top-level locator to HDS file

      CHARACTER*(DAT__SZLOC) BOTLOC  ! locator to HDS component

      LOGICAL VALID                  ! .TRUE. => a locator exists for
                                     ! the named parameter

*.


*
*   Use a new error reporting context.
*
      CALL EMS_BEGIN( STATUS )

*
*   Get the locator to the parameter's associated data.
*
      CALL SUBPAR_GETLOC ( NAMECODE, VALID, BOTLOC, STATUS )

      IF ( VALID ) THEN
*
*      There is an association to be cancelled
*      Annul locators associated with this parameter
*
         CALL HDS_FLUSH ( SUBPAR_PARGP(NAMECODE), STATUS )
*
*      If the parameter is not associated with the temporary
*      parameter storage data structure, close the container file.
*
         IF ( PARTYPE(NAMECODE) .GE. 20 ) THEN
            CALL SUBPAR_GETFLOC ( NAMECODE, VALID, FILOC, STATUS )
            CALL HDS_CLOSE ( FILOC, STATUS )
         ENDIF
*
*      Mark locators as not valid
*
         CALL SUBPAR_CANLOC ( NAMECODE, STATUS )

      ENDIF
*
*   Reset the parameter's state and type
*
      PARSTATE(NAMECODE) = SUBPAR__CANCEL
      PARTYPE(NAMECODE) = MOD ( PARTYPE(NAMECODE), 10 )
*
*   End the current error reporting environment, thus restoring any
*   original bad status, or retaing any new bad status if the original
*   status was good.
*
      CALL EMS_END( STATUS )

      END
