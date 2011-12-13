      SUBROUTINE SUBPAR_DATDEF ( NAMECODE, LOC, STATUS )
*+
*  Name:
*     SUBPAR_DATDEF

*  Purpose:
*     Set dynamic default of an HDS name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_DATDEF ( NAMECODE, LOC, STATUS )

*  Description:
*     The name of the HDS structure pointed at by the given locator is
*     set-up as the dynamic default for the indicated parameter.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        code-number of parameter
*     LOC=CHARACTER*(*) (given)
*        locator to HDS structure
*     STATUS=INTEGER

*  Algorithm:
*     HDS is requested for the container-file + structure-name
*     associated woth the given locator. These are concatenated and
*     stored as the dynamic default for the parameter.

*  Copyright:
*     Copyright (C) 1984, 1985, 1991, 1992, 1993 Science & Engineering Research Council.
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
*     10-DEC-1984 (BDK):
*        Original
*     05-JUN-1985 (BDK):
*        set PARDYN(2,NAMECODE)
*     16-JUL-1991 (AJC):
*        remove VMS library routines
*     22-JAN-1992 (AJC):
*        set SUBPAR not PARSE error
*     23-MAR-1992 (AJC):
*        re-use same space if possible
*      3-APR-1992 (AJC):
*        correctly use PARDYN(1,-) as space allocated flag
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
      INTEGER NAMECODE               ! code-number of parameter
      CHARACTER*(*) LOC              ! locator to HDS structure


*  Status:
      INTEGER STATUS


*  External References:
      INTEGER CHR_LEN


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      CHARACTER*132 PATH            ! HDS component name
      CHARACTER*132 FILE            ! HDS container file name
      INTEGER NLEV                  ! number of levels in structure
      INTEGER LENGTH                ! length of filename

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Find the HDS name of the object with the given locator
*
      CALL HDS_TRACE ( LOC, NLEV, PATH, FILE, STATUS )
      LENGTH = CHR_LEN ( FILE )
*
*   Concatenate the full name, and store it if there is enough room.
*
      IF ( ( PARDYN(1,NAMECODE) .GT. 0 ) .AND.
     :   ( ( PARDYN(3,NAMECODE) .EQ. -(20+MOD(PARTYPE(NAMECODE),10)) )
     :.OR. (PARDYN(3,NAMECODE) .EQ. (20+MOD(PARTYPE(NAMECODE),10)) ) ) )
     : THEN
*     Space is already reserved - set value and reset type
         CHARLIST ( PARDYN(1,NAMECODE) ) =
     :    '"' // FILE(1:LENGTH) // '"' // PATH
         PARDYN(3,NAMECODE) = 20 + MOD ( PARTYPE(NAMECODE), 10 )


      ELSEIF ( CHARPTR .LT. SUBPAR__MAXLIMS ) THEN
*     New space required - there is room
         CHARPTR = CHARPTR + 1
         CHARLIST ( CHARPTR ) = '"' // FILE(1:LENGTH) // '"' // PATH
         PARDYN(1,NAMECODE) = CHARPTR
         PARDYN(2,NAMECODE) = CHARPTR
         PARDYN(3,NAMECODE) = 20 + MOD ( PARTYPE(NAMECODE), 10 )

      ELSE
*     Run out of space in the common blocks
         STATUS = SUBPAR__DYNFULL
         CALL EMS_SETC ( 'NAME', PARKEY(NAMECODE) )
         CALL EMS_REP ( 'SUP_DATDEF1',
     :   'SUBPAR: Parameter ^NAME - '//
     :   'Store for dynamic defaults is full', STATUS )

      ENDIF

      END
