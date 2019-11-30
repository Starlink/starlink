      SUBROUTINE SUBPAR_STORE0( NAMECODE, ACTION, STRING, SLEN, LOC,
     :                          STATUS)
*+
*  Name:
*     SUBPAR_STORE0

*  Purpose:
*     Create storage and store a value, returning locator for storage

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_STORE0( NAMECODE, ACTION, STRING, SLEN, LOC, STATUS)

*  Description:
*     Create parameter storage of the correct type and store the value,
*     converting the given string to the required type.
*     Return a locator for the storage.

*  Arguments:
*     NAMECODE = INTEGER (given)
*        Name code for parameter
*     ACTION = INTEGER (given)
*        The LEX interpretation of type of value in the string
*     STRING = CHARACTER (given)
*        The string to be converted and stored
*     SLEN = INTEGER (given)
*        The length of the string to be used
*     LOC = CHARACTER*(DAT__SZLOC) (returned)
*        The locator for the storage
*     STATUS = INTEGER (given and returned)
*        Global status

*  Copyright:
*     Copyright (C) 1998, 1999 Central Laboratory of the Research Councils.
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
*     AJC: A J Chipperfield (STARLINK)

*  History:
*     16-DEC-1998 (AJC):
*        Original version
*     18-MAY-1999 (AJC):
*        Removed unused CHR_LEN
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
      INCLUDE 'LEX_PAR'

*  Arguments Given:
      INTEGER NAMECODE
      INTEGER ACTION
      CHARACTER*(*) STRING
      INTEGER SLEN

*  Arguments Returned:
      CHARACTER*(DAT__SZLOC) LOC

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  External routines :
      INTEGER STRING_IANYL       ! Index to character within string

*  Local Constants:
      CHARACTER*15 POSTYPES(8)   ! Possible primitive data types

*  Local Variables:
      INTEGER TYPE                ! Declared type of parameter
      CHARACTER*15 HDSTYPE        ! Type of object to create
      INTEGER TMPLEN              ! Size of _CHAR component
      INTEGER DIMS(2)             ! Dummy array dimensions array
      INTEGER I                   ! miscellaneous pointer

*  Local Data:
      DATA POSTYPES/'_CHAR*', '_REAL', '_DOUBLE', '_INTEGER',
     :     '_LOGICAL', ' ', ' ', '_INT64' /
*.

       IF ( STATUS .NE. SAI__OK ) RETURN

      TYPE = MOD( PARTYPE(NAMECODE), 10 )
      IF ( TYPE.GT.5 ) THEN
         STATUS = SUBPAR__IVPRTYPE
         CALL EMS_REP('SUP_STORE07',
     :     'SUBPAR_STORE0: Invalid parameter type - '
     :     //'system error', STATUS)

      ELSE
         IF ( TYPE .NE. SUBPAR__NOTYPE ) THEN
            HDSTYPE = POSTYPES(TYPE)
         ELSE IF ( ACTION .EQ. LEX__STRING ) THEN
            HDSTYPE = '_CHAR*'
         ELSE IF ( ACTION .EQ. LEX__INTEGER ) THEN
            HDSTYPE = '_INTEGER'
         ELSE IF ( ACTION .EQ. LEX__REAL ) THEN
            HDSTYPE = '_REAL'
         ELSE IF ( ACTION .EQ. LEX__DOUBLE ) THEN
            HDSTYPE = '_DOUBLE'
         ELSE IF ( ACTION .EQ. LEX__LOGICAL ) THEN
            HDSTYPE = '_LOGICAL'
         END IF

*     Make CHAR types size dependent
         IF ( HDSTYPE .EQ. '_CHAR*' ) THEN
            TMPLEN = MAX( 132, SLEN )
            CALL CHR_ITOC( TMPLEN, HDSTYPE(7:), I )
         END IF

*  Fix up D exponents if necessary so HDS will handle them
*  This should be fixed in HDS
         IF ( ( ACTION .EQ. LEX__DOUBLE )
     :   .AND.( HDSTYPE(1:5) .NE. '_CHAR' ) ) THEN
            I = STRING_IANYL( STRING(1:SLEN), 'Dd' )
            STRING(I:I) = 'E'
         END IF

*  Now create HDS component of the right type and store the value
         CALL SUBPAR_CRINT( NAMECODE, HDSTYPE, 0, DIMS, LOC,
     :      STATUS)
         CALL SUBPAR_PUT( LOC, HDSTYPE, 0, DIMS,
     :      STRING(1:SLEN), STATUS)
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL SUBPAR_CANCL( NAMECODE, STATUS )
         END IF

      END IF

      END
