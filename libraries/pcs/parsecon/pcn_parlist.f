      SUBROUTINE PARSECON_PARLIST ( ENTRY, STATUS )
*+
*  Name:
*     PARSECON_PARLIST

*  Purpose:
*     Insert value into parameter constraint list.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_PARLIST ( ENTRY, STATUS )

*  Description:
*     Adds a value into the constraint-list for the most recently
*     declared program parameter

*  Arguments:
*     ENTRY=CHARACTER*(*) (given)
*        data value
*     STATUS=INTEGER

*  Algorithm:
*     This routine adds a value into the constraint-list for the most recently
*     declared program parameter. The common variable PARPTR has been
*     previously set to point to the relevant parameter. The type of this
*     parameter is extracted from storage, and compared with the type deduced
*     from the syntax of the value string in ENTRY. Any valid type
*     conversions are performed, and the value is stored.

*  Copyright:
*     Copyright (C) 1984, 1988, 1990, 1991, 1993 Science & Engineering Research Council.
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
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14.09.1984:  Original (REVAD::BDK)
*     16.02.1988:  force character values to uppercase (REVAD::BDK)
*     16.10.1990:  Signal error on real or double to integer conversion
*        Use CHR for conversion (RLVAD::AJC)
*     25.02.1991:  Report errors (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PARSECON_ERR'
      INCLUDE 'PARSECON_PAR'
      INCLUDE 'SUBPAR_PAR'


*  Arguments Given:
      CHARACTER*(*) ENTRY


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      INTEGER DECTYPE                          ! code for type of
                                               ! declared program
                                               ! parameter

      INTEGER CLASS                            ! code for type of ENTRY
                                               ! deduced from its syntax

      CHARACTER*132 VALUE_CHAR                 ! decoded ENTRY string


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get the type declared for the current parameter
      DECTYPE = PARTYPE(PARPTR)

*   determine what type information can be deduced from the syntax of
*   the entry to be added, and do any syntax-dependent string
*   processing. - eg remove single quotes from character constant.
      CALL PARSECON_DECVAL ( ENTRY, VALUE_CHAR, CLASS, STATUS )

*   Check for acceptable type conversions between the given entry value
*   and the declared type of the associated action.
      IF ( CLASS .EQ. PARSE__STRUC ) THEN

*      Structures are not allowed as elements of a constraint list.
         STATUS = PARSE__IVLIST
         CALL EMS_REP ( 'PCN_PARLIST1',
     :   'PARSECON: Structures are not allowed in "NEEDS" constraints',
     :    STATUS )

      ELSE IF ( CLASS .EQ. PARSE__CHAR ) THEN

*      String constant. VALUE_CHAR will contain the string with the
*      delimiting single quotes removed, and double (ie escaped) quotes
*      contracted.
         IF ( DECTYPE .EQ. SUBPAR__CHAR ) THEN

            IF ( CHARPTR .LT. SUBPAR__MAXLIMS ) THEN

               CHARPTR = CHARPTR + 1
               CALL CHR_UCASE( VALUE_CHAR )
               CHARLIST(CHARPTR) = VALUE_CHAR
               IF ( PARLIMS(1,PARPTR) .EQ. 0 )
     :           PARLIMS(1,PARPTR) = CHARPTR
               PARLIMS(2,PARPTR) = CHARPTR

            ELSE

               STATUS = PARSE__NOMEM
               CALL EMS_REP ( 'PCN_PARLIST2',
     :         'PARSECON: Exceeded storage for CHARACTER constraints',
     :         STATUS )

            ENDIF

         ELSE

            STATUS = PARSE__VALTYPE
            CALL EMS_REP ( 'PCN_PARLIST3',
     :      'PARSECON: String as constraint for numeric parameter',
     :       STATUS )

         ENDIF

      ELSE IF ( CLASS .EQ. PARSE__NUMBER ) THEN

*      ENTRY contains a string with the syntax of a number. Convert this
*      to the relevant numeric type.
         IF ( DECTYPE .EQ. SUBPAR__REAL ) THEN

            IF ( REALPTR .LT. SUBPAR__MAXLIMS ) THEN

               REALPTR = REALPTR + 1
               CALL CHR_CTOR( ENTRY, REALLIST(REALPTR), STATUS )
               IF ( PARLIMS(1,PARPTR) .EQ. 0 )
     :           PARLIMS(1,PARPTR) = REALPTR
               PARLIMS(2,PARPTR) = REALPTR

            ELSE

               STATUS = PARSE__NOMEM
               CALL EMS_REP ( 'PCN_PARLIST4',
     :         'PARSECON: Exceeded storage for REAL constraints',
     :         STATUS )

            ENDIF

         ELSE IF ( DECTYPE .EQ. SUBPAR__DOUBLE ) THEN

            IF ( DOUBLEPTR .LT. SUBPAR__MAXLIMS ) THEN

               DOUBLEPTR = DOUBLEPTR + 1
               CALL CHR_CTOD( ENTRY, DOUBLELIST(DOUBLEPTR), STATUS )
               IF ( PARLIMS(1,PARPTR) .EQ. 0 )
     :           PARLIMS(1,PARPTR) = DOUBLEPTR
               PARLIMS(2,PARPTR) = DOUBLEPTR

            ELSE

               STATUS = PARSE__NOMEM
               CALL EMS_REP ( 'PCN_PARLIST5',
     :         'PARSECON: Exceeded storage for DOUBLE constraints',
     :         STATUS )

            ENDIF

         ELSE IF ( DECTYPE .EQ. SUBPAR__INTEGER ) THEN

            IF ( INTPTR .LT. SUBPAR__MAXLIMS ) THEN

               INTPTR = INTPTR + 1
               CALL CHR_CTOI( ENTRY, INTLIST(INTPTR), STATUS )
               IF ( PARLIMS(1,PARPTR) .EQ. 0 )
     :           PARLIMS(1,PARPTR) = INTPTR
               PARLIMS(2,PARPTR) = INTPTR

            ELSE

               STATUS = PARSE__NOMEM
               CALL EMS_REP ( 'PCN_PARLIST6',
     :         'PARSECON: Exceeded storage for INTEGER constraints',
     :         STATUS )

            ENDIF

         ELSE IF ( DECTYPE .EQ. SUBPAR__INT64 ) THEN

            IF ( INT64PTR .LT. SUBPAR__MAXLIMS ) THEN

               INT64PTR = INT64PTR + 1
               CALL CHR_CTOK( ENTRY, INT64LIST(INT64PTR), STATUS )
               IF ( PARLIMS(1,PARPTR) .EQ. 0 )
     :           PARLIMS(1,PARPTR) = INT64PTR
               PARLIMS(2,PARPTR) = INT64PTR

            ELSE

               STATUS = PARSE__NOMEM
               CALL EMS_REP ( 'PCN_PARLIST6',
     :         'PARSECON: Exceeded storage for INTEGER*8 constraints',
     :         STATUS )

            ENDIF

         ELSE IF ( DECTYPE .EQ. SUBPAR__LOGICAL ) THEN

*         Constraints not allowed on LOGICAL parameters
            STATUS = PARSE__IVLIST
            CALL EMS_REP ( 'PCN_PARLIST7',
     :      'PARSECON: Constraints not allowed on LOGICAL parameters',
     :       STATUS )

         ENDIF

         IF ( STATUS .EQ. SAI__ERROR ) THEN

            STATUS = PARSE__VALTYPE
            CALL EMS_REP ( 'PCN_PARLIST8',
     :      'PARSECON: Failed to convert item to required type',
     :       STATUS )

         ENDIF

      ELSE IF ( ( CLASS .EQ. PARSE__LOGTRUE ) .OR.
     :  ( CLASS .EQ. PARSE__LOGFALSE ) ) THEN

*      Logical values - not allowed
         STATUS = PARSE__IVLIST
         CALL EMS_REP ( 'PCN_PARLIST9',
     :   'PARSECON: LOGICAL constants are not allowed as constraints',
     :    STATUS )

      ELSE

         STATUS = PARSE__VALSYN
         CALL EMS_REP ( 'PCN_PARLIST10',
     :   'PARSECON: Invalid item in constraints list', STATUS )

      ENDIF

*   Set the data type of the constraint
      IF ( STATUS .EQ. SAI__OK ) THEN
         PARLIMS(3,PARPTR) = DECTYPE
      ENDIF

      END
