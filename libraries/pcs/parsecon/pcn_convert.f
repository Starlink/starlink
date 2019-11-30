      SUBROUTINE PARSECON_CONVERT ( STRING, INTYPE, RVAL, CVAL, DVAL,
     :  IVAL, I64VAL, LVAL, STRUCTURE, STATUS )
*+
*  Name:
*     PARSECON_CONVERT

*  Purpose:
*     Convert a string into a specified data type.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_CONVERT ( STRING, INTYPE, RVAL, CVAL, DVAL,
*    :  IVAL, I64VAL, LVAL, STRUCTURE, STATUS )

*  Description:
*     Given a character string and a requested data type, the syntax of
*     the string is checked, and if possible it is converted to the
*     requested type and the value returned in the corresponding
*     variable. If the string has a syntax corresponding to a data
*     structure name, then no conversion is attempted, and STRUCTURE is
*     set to .TRUE., unless the requested type is LITERAL.

*  Arguments:
*     STRING=CHARACTER*(*) (given)
*        character string to be converted
*     INTYPE=INTEGER (given)
*        type required, one of
*             SUBPAR__REAL
*             SUBPAR__CHAR
*             SUBPAR__DOUBLE
*             SUBPAR__INTEGER
*             SUBPAR__INT64
*             SUBPAR__LOGICAL
*             SUBPAR__LITERAL
*     RVAL=REAL (returned)
*        variable to hold converted value
*     CVAL=CHARACTER*(*) (returned)
*        variable to hold converted value
*     DVAL=DOUBLE PRECISION (returned)
*        variable to hold converted value
*     IVAL=INTEGER (returned)
*        variable to hold converted value
*     I64VAL=INTEGER*8 (returned)
*        variable to hold converted value
*     LVAL=LOGICAL (returned)
*        variable to hold converted value
*     STATUS=INTEGER

*  Algorithm:
*     The specified type is compared with the type deduced from the
*     syntax of the given string.
*     Any valid type conversions are performed, and the value is stored
*     in the corresponding variable.

*  Implementation Deficiencies:
*     The switch to using CHR for conversions will prevent a some
*     conversions which used to work, in particular, number to logical.
*     This is probably beneficial forcing stricter typing of values.

*  Copyright:
*     Copyright (C) 1984, 1985, 1990, 1991, 1992 Science & Engineering Research Council.
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
*     26.09.1984:  Original (REVAD::BDK)
*     18.04.1985:  Allow number<->character conversions (REVAD::BDK)
*     14.11.1985:  add type LITERAL (REVAD::BDK)
*     25.11.1985:  don't call DECVAL for LITERAL type (REVAD::BDK)
*     16.10.1990:  use CHR for type conversion  (RLVAD::AJC)
*     25.05.1991:  use CHR for finding start and end of STRING
*        use both in passing STRING down to subroutines (RLVAD::AJC)
*     26.02.1992:  report on errors (RLVAD::AJC)
*     12.06.1992:  allow string for universal type (RLVAD::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PARSECON_ERR'
      INCLUDE 'PARSECON_PAR'
      INCLUDE 'SUBPAR_PAR'


*  Arguments Given:
      CHARACTER*(*) STRING
      INTEGER INTYPE


*  Arguments Returned:
      REAL RVAL
      CHARACTER*(*) CVAL
      DOUBLE PRECISION DVAL
      INTEGER IVAL
      INTEGER*8 I64VAL
      LOGICAL LVAL
      LOGICAL STRUCTURE


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER CLASS                            ! code for type of STRING
                                               ! deduced from its syntax

      INTEGER START                            ! position of first
                                               ! significant character
      INTEGER END                              ! position of last
                                               ! significant character

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*  Skip leading spaces and tabs
*  We assume there is something there
      CALL CHR_FANDL ( STRING, START, END )

*  Literal - remove any surrounding quotes, and reduce any escaped quotes
      IF ( INTYPE .EQ. SUBPAR__LITERAL ) THEN
         CALL STRING_STRIPQUOT ( STRING(START:END), CVAL, STATUS )
         STRUCTURE = .FALSE.

*   Other - determine what type information can be deduced from the
*   syntax of STRING, and do any syntax-dependent string processing.
*   - eg remove single quotes from character constant.
      ELSE
         CALL PARSECON_DECVAL ( STRING(START:END), CVAL, CLASS,
     :    STATUS )

*     Check for acceptable type conversions between the given STRING value
*     and the requested type.

         IF ( CLASS .EQ. PARSE__STRUC ) THEN
*        Set the flag to indicate a data structure name, and do no further
*        processing.
            STRUCTURE = .TRUE.

         ELSE IF ( ( CLASS .EQ. PARSE__CHAR )
     :       .AND. ( INTYPE .EQ. SUBPAR__NOTYPE ) ) THEN
*        Assume it is a quoted name or name list
            STRUCTURE = .TRUE.

         ELSE
*        Attempt type conversion.
            STRUCTURE = .FALSE.

*        If the string has the syntax of a number, a string or a logical
*        constant, attempt type conversion
            IF ( CLASS .NE. PARSE__NONE ) THEN

               IF ( INTYPE .EQ. SUBPAR__REAL ) THEN
                  CALL CHR_CTOR( CVAL, RVAL, STATUS )

               ELSE IF ( INTYPE .EQ. SUBPAR__DOUBLE ) THEN
                  CALL CHR_CTOD( CVAL, DVAL, STATUS )

               ELSE IF ( INTYPE .EQ. SUBPAR__INTEGER ) THEN
                  CALL CHR_CTOI( CVAL, IVAL, STATUS )

               ELSE IF ( INTYPE .EQ. SUBPAR__INT64 ) THEN
                  CALL CHR_CTOK( CVAL, I64VAL, STATUS )

               ELSE IF ( INTYPE .EQ. SUBPAR__LOGICAL ) THEN
                  CALL CHR_CTOL( CVAL, LVAL, STATUS )

               ELSE IF ( INTYPE .EQ. SUBPAR__CHAR ) THEN
                  CONTINUE

               ELSE
*              Requires invalid conversion
                  STATUS = PARSE__VALTYPE
                  CALL EMS_REP ( 'PCN_CONVERT1',
     :            'PARSECON: Token requires invalid conversion',
     :             STATUS )
                  STRUCTURE = .TRUE.

               ENDIF

*           Check for CHR conversion error.
               IF ( STATUS .EQ. SAI__ERROR ) THEN
                  STATUS = PARSE__IVCONV
                  CALL EMS_REP ( 'PCN_CONVERT2',
     :            'PARSECON: Failed to convert to required type',
     :             STATUS )

               ENDIF

*        Any other string syntax is illegal here.
            ELSE
               STATUS = PARSE__VALSYN
               CALL EMS_SETC('TEXT', STRING(START:END) )
               CALL EMS_REP ( 'PCN_CONVERT3',
     :         'PARSECON: Token here may only be constant or name'/
     :         /' not "^TEXT"', STATUS )

            ENDIF

         ENDIF

      ENDIF

      END
