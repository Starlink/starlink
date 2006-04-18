      SUBROUTINE SUBPAR_CRINT ( NAMECODE, HDSTYPE, NDIMS, DIMS, LOC,
     :  STATUS )
*+
*  Name:
*     SUBPAR_CRINT

*  Purpose:
*     Create internal parameter storage.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_CRINT ( NAMECODE, HDSTYPE, NDIMS, DIMS, LOC,

*  Description:
*     Find or create internal parameter storage associated with a
*     parameter, set the parameter to indicate this, store the locators
*     with the parameter and set the parameter state to active.
*     If the routine fails, the locator will be nullified, ie annulled
*     and set to blank.

*  Arguments:
*     NAMECODE=INTEGER ( given)
*        pointer to the parameter
*     HDSTYPE=CHARACTER*(*) (given)
*        type of storage to be created
*     NDIMS=INTEGER (given)
*        number of dimensions required
*     DIMS(*)=INTEGER (given)
*        size of dimensions required
*     LOC=CHARACTER*(DAT__SZLOC) (returned)
*        locator to the created storage
*     STATUS=INTEGER

*  Algorithm:
*     The top-level locator to the program's private storage is obtained
*     at program activation and stored in common. The required space is
*     looked-for beneath this locator, with the same name as the parameter.
*     If the space doesn't already exist, then it is created. If it
*     exists but is of the wrong type or dimensions, then it is deleted
*     and re-created.

*  Copyright:
*     Copyright (C) 1984, 1985, 1990, 1991, 1993, 1994 Science & Engineering Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-SEP-1984 (BDK):
*        Original
*     22-MAR-1985 (BDK):
*        don't delete space if it matches requirement
*     02-FEB-1990 (AJC):
*        Guard against hanging locators
*     16-JUL-1991 (AJC):
*        don't LIB$SIGNAL error - use EMS
*     24-SEP-1991 (AJC):
*        prefix messages with 'SUBPAR:'
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     10-AUG-1993 (AJC):
*        Remove INCLUDE DAT_ERR
*      2-AUG-1994 (AJC):
*        Cancel parameter if active to avoid memory leak for I-tasks
*        (which do not close parameter system at end).
*      3-FEB-2000 (AJC):
*        Use SUBPAR_PARGP to get an HDS group name for the parameter
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! SAI Constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'


*  Arguments Given:
      INTEGER NAMECODE                  ! Parameter number

      CHARACTER*(*) HDSTYPE             ! type of storage to be created

      INTEGER NDIMS                     ! number of dimensions required

      INTEGER DIMS(*)               ! size of dimensions required


*  Arguments Returned:
      CHARACTER*(DAT__SZLOC) LOC        ! locator to the created storage

*    Status return :
      INTEGER STATUS                    ! Status Return


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  External Functions:
      CHARACTER*(DAT__SZGRP) SUBPAR_PARGP           ! HDS group name
      EXTERNAL SUBPAR_PARGP


*  Local Variables:
      CHARACTER*(DAT__SZLOC) TOPLOC

      CHARACTER*(DAT__SZLOC) BOTLOC

      LOGICAL THERE

      CHARACTER*(DAT__SZTYP) TTYPE      ! type of existing space

      INTEGER TNUM                      ! dimensionality of existing
                                        ! space

      INTEGER TDIMS(DAT__MXDIM)         ! dimensions of existing space

      INTEGER J                         ! loop counter

*.


      IF (STATUS .NE. SAI__OK) RETURN
*
*   If the parameter is active, cancel it - we are just about to set a new
*   value for it and mustn't retain existing locators.
      IF ( PARSTATE( NAMECODE ) .EQ. SUBPAR__ACTIVE )
     :   CALL SUBPAR_CANCL( NAMECODE, STATUS )
*
*   Initialise locator
*
      LOC = ' '
*   The top-level locator to private storage is obtained at program start-up
*   and is stored in a common block.
*
      TOPLOC = EXTLOC
      BOTLOC = ' '
*
*   Check whether the HDS storage already exists.
*
      CALL DAT_THERE ( TOPLOC, PARNAMES(NAMECODE), THERE, STATUS )

      IF ( THERE ) THEN
*
*      Check whether type and dimensions ok
*
         CALL DAT_FIND ( TOPLOC, PARNAMES(NAMECODE), BOTLOC, STATUS )
         CALL DAT_TYPE ( BOTLOC, TTYPE, STATUS )
         CALL DAT_SHAPE ( BOTLOC, DAT__MXDIM, TDIMS, TNUM, STATUS )

         IF ( ( TTYPE .NE. HDSTYPE ) .OR.
     :     ( TNUM .NE. NDIMS ) ) THEN

            CALL DAT_ANNUL ( BOTLOC, STATUS )
            CALL DAT_ERASE ( TOPLOC, PARNAMES(NAMECODE), STATUS )
            THERE = .FALSE.

         ELSE

            DO J = 1, TNUM
               IF ( TDIMS(J) .NE. DIMS(J) ) THERE = .FALSE.
            ENDDO

            IF ( .NOT. THERE ) THEN
               CALL DAT_ANNUL ( BOTLOC, STATUS )
               CALL DAT_ERASE ( TOPLOC, PARNAMES(NAMECODE), STATUS )
            ENDIF

         ENDIF

      ENDIF

      IF ( .NOT. THERE ) THEN
*
*      Create the HDS storage for the parameter value.
*
         BOTLOC = ' '
         CALL DAT_NEW ( TOPLOC, PARNAMES(NAMECODE), HDSTYPE, NDIMS,
     :     DIMS, STATUS )
         CALL DAT_FIND ( TOPLOC, PARNAMES(NAMECODE), BOTLOC, STATUS )

      ENDIF
*
*   Set-up the internal locator storage.
*
      CALL SUBPAR_PUTFLOC ( NAMECODE, TOPLOC, STATUS )
      CALL SUBPAR_PUTLOC ( NAMECODE, BOTLOC, STATUS )
      CALL HDS_LINK ( BOTLOC, SUBPAR_PARGP(NAMECODE), STATUS )
*
*   Return a copy of the locator
*
      CALL DAT_CLONE ( BOTLOC, LOC, STATUS )
      CALL HDS_LINK ( LOC, SUBPAR_PARGP(NAMECODE), STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
*
*   Set the state and type of the parameter
*
         PARSTATE(NAMECODE) = SUBPAR__ACTIVE
         PARTYPE(NAMECODE) = 10 + MOD ( PARTYPE(NAMECODE), 10 )

      ELSE

         CALL EMS_SETC( 'P', PARKEY(NAMECODE) )
         CALL EMS_REP( 'SUP_CRINT1',
     :   'SUBPAR: Failed to obtain parameter file component for '//
     :   'parameter ^P', STATUS )

      ENDIF

      END
