      SUBROUTINE PARSECON_INTERP ( STRING, TYPE, RVAL, CVAL,
     :  LVAL, STATUS )
*+
*  Name:
*     PARSECON_INTERP

*  Purpose:
*     Obsolete routine only needed for transfer vector.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_INTERP ( STRING, TYPE, RVAL, CVAL,
*    :   LVAL, STATUS )

*  Description:
*     Given a character string, the syntax of the string is checked,
*     and if possible it is converted to the corresponding primitive
*     data type and the value returned in the corresponding variable.

*  Arguments:
*     STRING=CHARACTER*(*) (given)
*        character string to be converted
*     TYPE=INTEGER (returned)
*        type identified, one of
*             SUBPAR__REAL
*             SUBPAR__CHAR
*             SUBPAR__LOGICAL
*     RVAL=REAL (returned)
*        variable to hold converted value
*     CVAL=CHARACTER*(*) (returned)
*        variable to hold converted value
*     LVAL=LOGICAL (returned)
*        variable to hold converted value
*     STATUS=INTEGER

*  Algorithm:
*     The type is deduced from the syntax of the given string.
*     Any valid type conversions are performed, and the value is stored
*     in the corresponding variable.

*  Copyright:
*     Copyright (C) 1985 Science & Engineering Research Council.
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
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     27.05.1985:  Original (REVAD::BDK)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
!      INCLUDE 'PARSECON_ERR'
!      INCLUDE 'PARSECON_PAR'
!      INCLUDE 'SUBPAR_PAR'
!      INCLUDE '($SSDEF)'


*  Arguments Given:
      CHARACTER*(*) STRING


*  Arguments Returned:
      INTEGER TYPE                             ! code for type of STRING
                                               ! deduced from its syntax

      REAL RVAL                                ! value if real

      CHARACTER*(*) CVAL                       ! value if character

      LOGICAL LVAL                             ! value if logical


*  Status:
      INTEGER STATUS


*  External References:
!      INTEGER LIB$CVT_DX_DX
!      EXTERNAL LIB$CVT_DX_DX


*  Local Variables:
!      INTEGER ISTAT                            ! status return from
                                               ! type-conversion utility


*.


      IF ( STATUS .NE. SAI__OK ) RETURN
!
!
!*
!*   Determine what type information can be deduced from the syntax of
!*   STRING, and do any syntax-dependent string processing. - eg remove
!*   single quotes from character constant.
!*
!      CALL PARSECON_DECVAL ( STRING, CVAL, TYPE, STATUS )
!*
!*   Convert the string to the variable of corresponding type.
!*
!      IF ( TYPE .EQ. PARSE__CHAR ) THEN
!
!         CONTINUE
!
!      ELSE IF ( TYPE .EQ. PARSE__NUMBER ) THEN
!
!         ISTAT = LIB$CVT_DX_DX ( %DESCR(CVAL),
!     :     %DESCR(RVAL) )
!
!         IF ( ISTAT .NE. SS$_NORMAL ) THEN
!            STATUS = PARSE__IVCONV
!         ENDIF
!
!      ELSE IF ( TYPE .EQ. PARSE__LOGTRUE ) THEN
!
!         LVAL = .TRUE.
!
!      ELSE IF ( TYPE .EQ. PARSE__LOGFALSE ) THEN
!
!         LVAL = .FALSE.
!
!      ELSE
!
!         STATUS = PARSE__VALSYN
!
!      ENDIF
!
      END
