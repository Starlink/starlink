      SUBROUTINE SUBPAR_CMDPAR(NAMECODE, VALUE, STATUS)
*+
*  Name:
*     SUBPAR_CMDPAR

*  Purpose:
*     Set a parameter value based on a command string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_CMDPAR ( NAMECODE, VALUE, STATUS )

*  Description:
*     The character string VALUE is used to derive a value
*     for a parameter.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to a parameter
*     VALUE=CHARACTER*(*)  (given)
*        String to be used to derive value
*     STATUS=INTEGER

*  Algorithm:
*     A preliminary parse of the string is used to determine whether
*     it is a single item or a list of items. If it is a list of items
*     it is interpreted as an array. The string is then parsed using
*     the LEX parser, an action code is returned for each item and
*     used to determine how the parameter value should be written.

*  Implementation Deficiencies:
*     Setting STATUS to SAI__OK is believed to be an error. The parameter
*     may not have been set. This needs investigation. (AJC)

*  Copyright:
*     Copyright (C) 1987, 1988, 1991, 1992, 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 1998, 2000 Central Laboratory of the Research Councils.
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
*     JAB: J A Bailey (AAO)
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-JAN-1987 (JAB):
*        revised error codes
*     30-JAN-1987 (JAB):
*        Add special case for _CHAR type
*     30-JAN-1987 (JAB):
*        Allow lower case logical constants
*     14-MAY-1987 (BDK):
*        change include name to LEX_ERR
*     03-JUL-1988 (JAB):
*        Allow directory specs in [ ] to be distinguished
*        from arrays
*     08-DEC-1988 (AJC):
*        Correct test for logical false lower case
*        Permit ]] and ][ in arrays during check for directory spec
*        Work with 'used' length of COMMAND
*        Use LEX_PAR symbolic constants
*     20-DEC-1988 (AJC):
*        Re-write tests for logical constants to allow mixed
*        case
*     22-JUL-1991 (AJC):
*        Remove unused declaration INIT
*     24-SEP-1991 (AJC):
*        Correctly handle error reports
*     20-JUL-1991 (AJC):
*        Move DATA statement
*     16-NOV-1992 (AJC):
*        Add MIN/MAX parameter state
*        Add Null specifier
*        Improve comments round testing for single or multiple items
*        Skip out if empty command line - avoids potential string bounds fault
*        Remove multiple setting TYPE
*        Remove outer loop on PARSET
*        Handle arrays with SUBPAR_ARRAY
*        Fix directory specs with SUBPAR_DIRFX
*     10-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*      7-MAY-1993 (AJC):
*        Fix up D exponents so HDS can handle them
*        Remove unused variables etc
*      5-AUG-1994 (AJC):
*        Cancel before setting MIN, MAX or NULL state
*     16-DEC-1998 (AJC):
*        Remember value if MIN/MAX.
*     21-MAR-2000 (AJC):
*        Remove the trapping of directories in [] - VMS requirement
*        causes problems with GRP
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
      INCLUDE 'LEX_ERR'
      INCLUDE 'LEX_PAR'


*  Arguments Given:
      INTEGER NAMECODE          ! pointer to a parameter
      CHARACTER*(*) VALUE       ! string used to derive value


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  External routines:
      INTEGER CHR_LEN            ! used length of string
      INTEGER STRING_IANYL       ! Index of character within string


*  Local Constants:
      INTEGER MCLENGTH           ! maximum length of string
      PARAMETER (MCLENGTH=200)
      CHARACTER*15 POSTYPES(8)   ! Possible primitive data types

*  Local Variables:
      CHARACTER*(MCLENGTH+3) COMMAND  ! command string with SPACE CR added
      INTEGER ENDLINE             ! Position of end of command line
      INTEGER ACTION              ! Action code from parser
      CHARACTER*(MCLENGTH) STRING ! Parameter string from parser
      INTEGER SLEN                ! length of above
      CHARACTER*(SUBPAR__NAMELEN) USTRING ! upper case of STRING
      INTEGER I, J                ! Loop counters
      INTEGER TYPE                ! type code for parameter
      CHARACTER*15 HDSTYPE        ! HDS type string
      CHARACTER*(DAT__SZLOC) LOC  ! HDS locator
      INTEGER DIMS(2)             ! Dummy array dimensions
      INTEGER LEVEL               ! Current level in array
      INTEGER STAT                ! Temporary status
      INTEGER NPARS               ! Count of items in list
      LOGICAL SINGLE              ! True if can be interpreted as single
      LOGICAL LVALUE              ! Conversion of logical constant

*  Local Data:
      DATA POSTYPES /'_CHAR*132', '_REAL', '_DOUBLE', '_INTEGER',
     :   '_LOGICAL', ' ', ' ', '_INT64' /

*.


      IF (  STATUS .NE. SAI__OK ) RETURN

*  Set error context
      CALL EMS_MARK

*  Find length of command line (+2 for termination)
      ENDLINE = MIN( MCLENGTH, CHR_LEN(VALUE) ) + 2

*  If there is anything on the command line, process it
      IF ( ENDLINE.GT.2 ) THEN

*     Add a CR at end of command line
         COMMAND = VALUE(1:ENDLINE-2)//' '//CHAR(13)

*  Find out if there is more than one item in the list
*  by means of a preliminary parse of the string

         NPARS = 0
         LEVEL = 0
         CALL LEX_CMDLINE( .TRUE., COMMAND(1:ENDLINE), ACTION, STRING,
     :                    SLEN, STATUS )

*      If the first item is a quoted string or [, the value cannot be an
*      unquoted multi-word value, even for a parameter of type CHAR, therefore
*      set SINGLE false; otherwise it is set true.
         IF ( ( ACTION .NE. LEX__STRING )
     :  .AND. ( ACTION .NE. LEX__STARR ) ) THEN
            SINGLE = .TRUE.
         ELSE
            SINGLE = .FALSE.
         END IF

*      Get the declared type of the parameter
         TYPE = MOD( PARTYPE(NAMECODE), 10 )

*      For types other than CHAR (and for CHAR if the first item of the value
*      was a quoted string or [)
         IF ( ( TYPE .NE. SUBPAR__CHAR ) .OR. .NOT.SINGLE ) THEN

*        check if there is more than one item - an item possibly having the
*        form of a bracketed array.
*        Only count until more than one
            DO WHILE ( ( STATUS .EQ. SAI__OK )
     :           .AND. ( ACTION.LE.LEX__ELINE )
     :           .AND. ( NPARS .LT. 2 ) )

               IF ( ( ACTION .EQ. LEX__STRING )
     :         .OR. ( ACTION .EQ. LEX__INTEGER )
     :         .OR. ( ACTION .EQ. LEX__REAL )
     :         .OR. ( ACTION .EQ. LEX__AMBIG )
     :         .OR. ( ACTION .EQ. LEX__KAMBIG )
     :         .OR. ( ACTION .EQ. LEX__DOUBLE ) ) THEN
                  IF ( LEVEL .EQ. 0 ) NPARS = NPARS + 1

*           If '[' maintain the level count
               ELSE IF ( ACTION .EQ. LEX__STARR ) THEN
                  LEVEL = LEVEL + 1
*              and count open brackets at the top level
                  IF ( LEVEL .EQ. 1 ) NPARS = NPARS + 1

*           If ']' maintain the level count
               ELSE IF ( ACTION .EQ. LEX__ENDARR ) THEN
                  LEVEL = LEVEL - 1
               END IF

*           Get next item
               CALL LEX_CMDLINE( .FALSE., COMMAND(1:ENDLINE), ACTION,
     :                          STRING, SLEN, STATUS )
            END DO
         END IF

*  Reset status in case it changed

         CALL EMS_ANNUL( STATUS )

*  If more than one item add the outer brackets to intepret as an array

         IF ( NPARS .GT. 1 ) COMMAND = '['//VALUE(1:ENDLINE-2)
     :                               //']'//CHAR(13)


*  initiate parse
         CALL LEX_CMDLINE( .TRUE., COMMAND(1:ENDLINE), ACTION, STRING,
     :                    SLEN, STATUS )

*   If OK, process the string
         IF ( STATUS .EQ. SAI__OK ) THEN

*   Special case for type _CHAR
*   Treat number(s) as string (possibly multiple word)
            IF ( ( TYPE .EQ. SUBPAR__CHAR )
     :       .AND. SINGLE
     :       .AND. ( ( ACTION .EQ. LEX__INTEGER )
     :          .OR. ( ACTION .EQ. LEX__REAL )
     :          .OR. ( ACTION .EQ. LEX__DOUBLE ) ) ) THEN

*         Set STRING to whole of given value
               STRING = VALUE(1:ENDLINE-2)
               SLEN = ENDLINE - 2
*        Interpret as a string
               ACTION = LEX__STRING
            END IF


*   First deal with the ambiguous action codes
            IF ( ( ACTION .EQ. LEX__AMBIG )
     :      .OR. ( ACTION .EQ. LEX__KAMBIG ) ) THEN
*   String or Name or Logical Constant or MIN/MAX or !

*           Get an upper case copy of STRING
               USTRING = STRING(1:SUBPAR__NAMELEN)
               CALL CHR_UCASE( USTRING )

*           Check for MIN or MAX
               IF ( ( USTRING(1:SLEN) .EQ. 'MAX' ) .OR.
     :              ( USTRING(1:SLEN) .EQ. 'MIN' ) ) THEN

                  IF ( ( TYPE .EQ. SUBPAR__REAL )
     :            .OR. ( TYPE .EQ. SUBPAR__INTEGER )
     :            .OR. ( TYPE .EQ. SUBPAR__DOUBLE )
     :            .OR. ( TYPE .EQ. SUBPAR__CHAR ) ) THEN
*              Cancel any existing value
*              Set the appropriate state and for character parameters
*              save the string so it can be used with correct case if
*              there is no MIN/MAX defined when it's used.
                     CALL SUBPAR_CANCL( NAMECODE, STATUS )
                     IF ( USTRING(1:SLEN) .EQ. 'MAX' ) THEN
                        PARSTATE(NAMECODE) = SUBPAR__MAX
                        IF ( TYPE .EQ. SUBPAR__CHAR )
     :                     PARVALS(NAMECODE) = STRING
                     ELSE
                        PARSTATE(NAMECODE) = SUBPAR__MIN
                        IF ( TYPE .EQ. SUBPAR__CHAR )
     :                     PARVALS(NAMECODE) = STRING
                     END IF
*                 No further action is required
                     ACTION = 0

*              Otherwise treat MIN/MAX as a name
                  ELSE
                     ACTION = LEX__NAME

                  END IF

*           Check for NULL specifier
               ELSE IF ( STRING(1:SLEN) .EQ. '!' ) THEN
*              Set null state - cancel any existing value first
                  CALL SUBPAR_CANCL( NAMECODE, STATUS )
                  PARSTATE(NAMECODE) = SUBPAR__NULL
*              and no further action
                  ACTION = 0

               ELSE IF ( TYPE .EQ. SUBPAR__CHAR ) THEN
*              Interpret as literal string
*              Possibly multi-word
                  IF ( SINGLE ) THEN
                     STRING = VALUE(1:ENDLINE-2)
                     SLEN = ENDLINE - 2
                  END IF
                  ACTION = LEX__STRING

               ELSE IF ( ( TYPE .EQ. SUBPAR__LOGICAL )
     :              .OR. ( TYPE .EQ. SUBPAR__NOTYPE ) ) THEN
*              Attempt to convert the string to logical
                  STAT = SAI__OK
                  CALL CHR_CTOL( STRING(1:SLEN), LVALUE, STAT )

                  IF ( STAT .EQ. SAI__OK ) THEN
*                 Interpret as logical constant
                     ACTION = LEX__LOGICAL
                     SLEN = 1
                     IF ( LVALUE ) THEN
                        STRING = 'T'
                     ELSE
                        STRING = 'F'
                     END IF

                  ELSE
*                 Not valid logical constant
*                 Interpret as a name
                     ACTION = LEX__NAME
                  END IF

               ELSE
*              Not one of the special cases
*              Interpret as a  name
                  ACTION = LEX__NAME
               END IF

            END IF


*  Having resolved ambiguities, perform the required action

            IF ( ACTION .EQ. LEX__NAME ) THEN
*  Name (e.g. HDS or device name or null specifier)
               CALL SUBPAR_PUTNAME( NAMECODE, STRING(1:SLEN), STATUS )

*   Primitive item (String, Integer, Real, Double Precision, Logical)
*   All these can be handled by the same code as SUBPAR_PUT0C
*   will do any necessary conversion

            ELSE IF ( ( ACTION .EQ. LEX__STRING )
     :           .OR. ( ACTION .EQ. LEX__INTEGER )
     :           .OR. ( ACTION .EQ. LEX__REAL )
     :           .OR. ( ACTION .EQ. LEX__DOUBLE )
     :           .OR. ( ACTION .EQ. LEX__LOGICAL ) ) THEN

*  If parameter is internal cancel any previous HDS association
*  and store the value
               IF ( PARVPATH(1,NAMECODE) .EQ. SUBPAR__INTERNAL ) THEN
                  CALL SUBPAR_CANCL( NAMECODE, STATUS )
                  CALL SUBPAR_PUT0C( NAMECODE, STRING(1:SLEN), STATUS )

*  If not internal create an HDS component of the same type as
*  the parameter and put the value in it. If the parameter
*  has a primitive type as defined in the interface file
*  use that type - Otherwise use the type derived from the
*  parsing of the string
               ELSE IF ( TYPE .GT. 5 ) THEN
                  STATUS = SUBPAR__IVPRTYPE
                  CALL EMS_REP( 'SUP_CMDPAR1',
     :                       'SUBPAR_CMDPAR: Invalid parameter type - '
     :                       //'system error', STATUS )
               ELSE
                  IF ( TYPE .NE. SUBPAR__NOTYPE ) THEN
                     HDSTYPE = POSTYPES( TYPE)
                  ELSE IF ( ACTION .EQ. LEX__STRING ) THEN
                     HDSTYPE = '_CHAR*132'
                  ELSE IF ( ACTION .EQ. LEX__INTEGER ) THEN
                     HDSTYPE = '_INTEGER'
                  ELSE IF ( ACTION .EQ. LEX__REAL ) THEN
                     HDSTYPE = '_REAL'
                  ELSE IF ( ACTION .EQ. LEX__DOUBLE ) THEN
                     HDSTYPE = '_DOUBLE'
                  ELSE IF ( ACTION .EQ. LEX__LOGICAL ) THEN
                     HDSTYPE = '_LOGICAL'
                  END IF

*  Fix up D exponents if necessary so HDS will handle them
*  This should be fixed in HDS
                  IF ( ( ACTION .EQ. LEX__DOUBLE )
     :            .AND.( HDSTYPE(1:5) .NE. '_CHAR' ) ) THEN
                     I = STRING_IANYL( STRING(1:SLEN), 'Dd' )
                     STRING(I:I) = 'E'
                  END IF

*  Now create HDS component of the right type and store the value
                  CALL SUBPAR_CRINT( NAMECODE, HDSTYPE, 0, DIMS, LOC,
     :                              STATUS )
                  CALL SUBPAR_PUT( LOC, HDSTYPE, 0, DIMS,
     :                             STRING(1:SLEN), STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL SUBPAR_CANCL( NAMECODE, STATUS )
                  END IF
                  CALL DAT_ANNUL( LOC, STATUS )

               END IF


            ELSE IF ( ACTION .EQ. LEX__KEYWORD ) THEN
*  Keyword
               STATUS = SUBPAR__CMDSYER
               CALL EMS_REP( 'SUP_CMDPAR2',
     :                      'SUBPAR: unquoted ''='' is not allowed',
     :                      STATUS )

            ELSE IF ( ACTION .EQ. LEX__STARR ) THEN
*  Array
               CALL SUBPAR_ARRAY( NAMECODE, COMMAND(1:ENDLINE), LOC,
     :                           STATUS )
*           The returned locator is not used
               IF ( STATUS .EQ. SAI__OK ) CALL DAT_ANNUL( LOC, STATUS )

            ELSE IF ( ACTION .NE. 0 ) THEN
*  At this stage, any other ACTION is an error
               STATUS = SUBPAR__CMDSYER
               CALL EMS_SETC( 'STRING', STRING(1:SLEN) )
               CALL EMS_REP( 'SUP_CMDPAR3',
     :              'SUBPAR: Command line syntax error /^STRING/',
     :              STATUS )
            END IF

         END IF

         IF ( STATUS .EQ. LEX__ENDPARSE ) THEN
            CALL EMS_ANNUL( STATUS )

         ELSE IF ( STATUS .EQ. LEX__INVCHAR ) THEN
*        Change to a SUBPAR error code if invalid character found
            STATUS = SUBPAR__CMDSYER
            CALL EMS_REP( 'SUP_CMDPAR4',
     :          'SUBPAR: Syntax error in parsing given parameter value',
     :          STATUS )
         END IF

      END IF

*  End error context
      CALL EMS_RLSE

      END
