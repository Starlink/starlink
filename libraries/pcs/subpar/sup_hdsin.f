      SUBROUTINE SUBPAR_HDSIN(NAMECODE, ACCESS, LOC, STATUS)
*+
*  Name:
*     SUBPAR_HDSIN

*  Purpose:
*     Get input from a user and return a locator to it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_HDSIN ( NAMECODE, ACCESS, LOC, STATUS )

*  Description:
*     The user is prompted for input, and a character-string reply is
*     received. This is used to determine the value of the parameter
*     and a locator to the parameter is returned

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to a parameter
*     ACCESS=CHARACTER*(*) (given)
*        access required to the HDS structure
*     LOC=CHARACTER*(DAT__SZLOC) (returned)
*        locator to the stored data
*     STATUS=INTEGER

*  Algorithm:
*     Pre-parse to replace directory specs in [] to ones in <>
*     A preliminary parse of the string is used to determine whether
*     it is a single item or a list of items. If it is a list of items
*     it is interpreted as an array. The string is then parsed using
*     the LEX parser, an action code is returned for each item and
*     used to determine how the parameter value should be intepreted.

*  Copyright:
*     Copyright (C) 1984, 1985, 1986, 1987, 1988, 1991, 1992, 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 1999, 2000 Central Laboratory of the Research Councils.
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
*     JAB: J A Bailey (AAO)
*     AJC: A J Chipperfield (STARLINK)
*     DSB: D S Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     26-SEP-1984 (BDK):
*        Original
*     27-MAY-1985 (BDK):
*        Store values even when type not primitive
*        - eg. 'UNIV'
*     14-NOV-1985 (BDK):
*        recognise LITERAL parameters
*     09-MAY-1986 (BDK):
*        set COUNT=1 for LITERALs
*     02-SEP-1986 (BDK):
*        check for HDS name before array
*     16-JAN-1987 (JAB):
*        Complete rewrite to use LEX parser
*     16-JAN-1987 (JAB):
*        Call SUBPAR_CANCL after a conversion failure
*     21-JAN-1987 (JAB):
*        revised error codes
*     30-JAN-1987 (JAB):
*        Add special case for CHAR type
*     30-JAN-1987 (JAB):
*        Allow lower case logical constants
*     05-MAY-1987 (JAB):
*        Check status from SUBPAR_INPUT
*     14-MAY-1987 (BDK):
*        change include name to LEX_ERR
*     03-JUL-1988 (JAB):
*        Allow directory specs in [ ] to be distinguished
*        from arrays
*     05-DEC-1988 (AJC):
*        Allow for ] followed by ] or [ in check for directory spec.
*        Specify length of COMMAND to LEX_CMDLINE
*        (insert spaceCR to prevent LEX__ENDPARSE).
*        Do not reset LEX__ENDPARSE error to OK at end but do it
*        after getting array
*        Use LEX_PAR symbolic constants
*     20-DEC-1988 (AJC):
*        Re-write tests for logical constants to allow mixed
*        case
*     22-JUL-1991 (AJC):
*        Remove unused declaraction INIT
*     23-SEP-1991 (AJC):
*        Correctly handle error reports
*     26-SEP-1991 (AJC):
*        Remove unauthorised RETURN statement
*     20-JUL-1992 (AJC):
*        Move DATA statement
*     16-NOV-1992 (AJC):
*        Add MIN/MAX
*        Improve comments round testing for single or multiple items
*        Skip out if empty command line - avoids potential string bounds fault
*        Remove multiple setting TYPE
*        Remove outer loop on PARSET
*        Handle arrays with SUBPAR_ARRAY
*        Fix directory specs with SUBPAR_DIRFX
*     10-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*     24-MAR-1993 (AJC):
*        Allow = in string in response to prompt for CHAR
*      7-MAY-1993 (AJC):
*        Fix up D exponents so HDS can handle them
*        Remove unused variables etc
*      4-JUN-1993 (AJC):
*        Correctly check type _CHAR for MIN/MAX
*      8-AUG-1994 (AJC):
*        Correctly handle error from SUBPAR_MNMX
*     28-JUN-1995 (AJC):
*        Remove extra comma from EMS_REP call
*     20-DEC-1995 (AJC):
*        Create Parameter file component of appropriate size for CHAR
*        (min 132).
*        Allow input string up to 444 chars (was 200).
*     16-DEC-1998 (AJC):
*        Assume string if no MIN/MAX for character parameter.
*     18-MAY-1999 (AJC):
*        Remove unused variables and commented out code
*      3-FEB-2000 (AJC):
*        Use SUBPAR_PARGP to get an HDS group name for the parameter
*     21-MAR-2000 (AJC):
*        Remove the trapping of directories in [] - VMS requirement
*        causes problems with GRP
*     28-JUL-2017 (DSB):
*        Use MESSYS__VAL_LEN (now 1944) to define the max length of a
*        command line, instead of the local parameter MCLENGTH (was 444).
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
      INCLUDE 'MESSYS_PAR'

*  Arguments Given:
      INTEGER NAMECODE          ! pointer to a parameter
      CHARACTER*(*) ACCESS      ! access required to the HDS structure

*  Arguments Returned:
      CHARACTER*(DAT__SZLOC) LOC  ! locator to the stored data

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*    External routines :
      INTEGER CHR_LEN

*  Local Constants:
      CHARACTER*15 POSTYPES(5)   ! Possible primitive data types

*  External Functions:
      CHARACTER*(DAT__SZGRP) SUBPAR_PARGP           ! HDS group name
      EXTERNAL SUBPAR_PARGP


*  Local Variables:
      CHARACTER*(MESSYS__VAL_LEN) VALUE  ! Returned string
      CHARACTER*(MESSYS__VAL_LEN+3) COMMAND  ! Returned string with SPACE CR added
      INTEGER ENDLINE             ! Length of command line
      INTEGER ACTION              ! Action code from parser
      CHARACTER*(MESSYS__VAL_LEN) STRING ! Parameter string from parser
      CHARACTER*(SUBPAR__NAMELEN) USTRING ! Upper case of STRING
      INTEGER SLEN                ! length of above
      INTEGER J                   ! Loop counters
      INTEGER TYPE                ! type code for parameter
      INTEGER LEVEL               ! Current level in array
      INTEGER STAT                ! Temporary status
      INTEGER NPARS               ! Count of items in list
      LOGICAL SINGLE              ! TRUE if can be interpreted as a sing
      LOGICAL LVALUE              ! Conversion of logical constant

*  Local Data:
      DATA POSTYPES /'_CHAR*', '_REAL', '_DOUBLE', '_INTEGER',
     :   '_LOGICAL' /

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set an error context
      CALL EMS_MARK

*  Get input from user
      CALL SUBPAR_INPUT( NAMECODE, VALUE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Add  space and CR at end of string for LEX
*  space to terminate tokens correctly and CR to terminate line
         ENDLINE = CHR_LEN( VALUE ) + 2

         IF ( ENDLINE .GT. 2 ) THEN

*        Add space CR at end of line
            COMMAND = VALUE(1:ENDLINE-2)//' '//CHAR(13)

*  Find out if there is more than one item in the list
*  by means of a preliminary parse of the string

            NPARS = 0
            LEVEL = 0
            CALL LEX_CMDLINE( .TRUE., COMMAND(1:ENDLINE), ACTION,
     :                       STRING, SLEN, STATUS )

*      If the first item is a quoted string or [, the value cannot be an
*      unquoted multi-word value, even for a parameter of type CHAR, therefore
*      set SINGLE false; otherwise it is set true.
            IF ( (ACTION .NE. LEX__STRING )
     :     .AND. (ACTION .NE. LEX__STARR ) ) THEN
               SINGLE = .TRUE.
            ELSE
               SINGLE = .FALSE.
            END IF

*      Get the declared type of the parameter
            TYPE = MOD( PARTYPE(NAMECODE), 10 )

*      For types other than CHAR (and for CHAR if the first item of the value
*      was a quoted string or [)
            IF ( ( TYPE .NE. SUBPAR__CHAR ) .OR. .NOT. SINGLE ) THEN

*        check if there is more than one item - an item possibly having the
*        form of a bracketed array.
*        Only count until more than one
               DO WHILE ( ( STATUS .EQ. SAI__OK )
     :              .AND. ( ACTION.LE.LEX__ELINE )
     :              .AND. ( NPARS.LT.2 ) )

                  IF ( ( ACTION .EQ. LEX__STRING )
     :            .OR. ( ACTION .EQ. LEX__INTEGER )
     :            .OR. ( ACTION .EQ. LEX__REAL )
     :            .OR. ( ACTION .EQ. LEX__AMBIG )
     :            .OR. ( ACTION .EQ. LEX__KAMBIG )
     :            .OR. ( ACTION .EQ. LEX__DOUBLE ) ) THEN
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
     :                             STRING, SLEN, STATUS )
               END DO
            END IF

*  Reset status in case it changed

            CALL EMS_ANNUL( STATUS )

*  If more than one item add the outer brackets to intepret as an array

            IF ( NPARS .GT. 1 ) THEN
               COMMAND = '['//VALUE(1:ENDLINE-2)//']'//CHAR(13)
               ENDLINE = ENDLINE + 1
            END IF


*  initiate parse

            CALL LEX_CMDLINE( .TRUE., COMMAND(1:ENDLINE), ACTION,
     :                       STRING, SLEN, STATUS )

*        If OK, process the string obtained
            IF ( STATUS .EQ. SAI__OK ) THEN

*   By this time, unbracketed arrays have been bracketed so, unless it
*   is the start of an array or a quoted string or a forced name, the whole
*   line may be treated as one string
               IF ( ( ACTION .NE. LEX__STARR )
     :         .AND.( ACTION .NE. LEX__STRING )
     :         .AND.( ACTION .NE. LEX__NAME ) ) THEN
*           Set STRING to whole of given value
                  STRING = VALUE(1:ENDLINE-2)
                  SLEN = ENDLINE - 2
*           The action can remain unchanged
               END IF

*   First deal with the ambiguous action codes
               IF ( ( ACTION .EQ. LEX__AMBIG )
     :         .OR. ( ACTION .EQ. LEX__KAMBIG ) ) THEN

*              String or Name or Logical Constant or MIN/MAX
*              Get an upper case copy of the string for testing against MIN/MAX
                  USTRING = STRING(1:SLEN)
                  CALL CHR_UCASE( USTRING(1:3) )

*              Handle MIN/MAX as a special case except for type LOGICAL
*              and non-primitive.
                  IF ( ( TYPE .NE. SUBPAR__LOGICAL ) .AND.
     :              ( TYPE .NE. SUBPAR__NOTYPE ) .AND.
     :              ( ( USTRING(1:SLEN) .EQ. 'MIN' ) .OR.
     :                ( USTRING(1:SLEN) .EQ. 'MAX' ) ) ) THEN
*                 Put the required limit as the parameter value
                     CALL SUBPAR_MNMX( NAMECODE, USTRING(1:SLEN),
     :                              STATUS )
*                 and check if done
                     IF ( STATUS .EQ. SAI__OK ) THEN
*                    if so, obtain the locator.
                        CALL DAT_CLONE( PARLOC(2,NAMECODE), LOC,
     :                               STATUS )
                        CALL HDS_LINK( LOC, SUBPAR_PARGP(NAMECODE),
     :                              STATUS )
                     ELSEIF( ( STATUS .EQ. SUBPAR__NOMNMX ) .AND.
     :                       ( TYPE .EQ. SUBPAR__CHAR ) ) THEN
                        CALL EMS_ANNUL( STATUS )
                        CALL SUBPAR_STORE0( NAMECODE, LEX__STRING,
     :                     STRING, SLEN, LOC, STATUS )
                     ENDIF
*                 and signal action done
                     ACTION = 0

*                 If it wasn't MIN/MAX for numeric or _CHAR type
*                 and the type is LOGICAL or non-primitive,
*                 check if it's a logical constant
                  ELSE IF ( ( TYPE .EQ. SUBPAR__LOGICAL )
     :                 .OR. ( TYPE .EQ. SUBPAR__NOTYPE ) ) THEN
*                 Attempt to convert the string to logical
                     STAT = SAI__OK
                     CALL CHR_CTOL( STRING(1:SLEN), LVALUE, STAT )
                     IF ( STAT .EQ. SAI__OK ) THEN
*                    Interpret as logical constant
                        ACTION = LEX__LOGICAL
                        SLEN = 1
                        IF ( LVALUE ) THEN
                           STRING = 'T'
                        ELSE
                           STRING = 'F'
                        END IF

                     ELSE
*                    Not valid logical constant
*                    Interpret as a name
                        ACTION = LEX__NAME
                     END IF

*              Otherwise, if type is CHAR, assume we have a string
                  ELSE IF ( TYPE .EQ. SUBPAR__CHAR ) THEN
                     ACTION = LEX__STRING

*              Not one of the special cases
*              Interpret as a  name
                  ELSE
                     ACTION = LEX__NAME
                  END IF

               END IF


               IF ( ACTION .EQ. LEX__NAME ) THEN

*           Name (e.g. HDS or device name)
                  CALL SUBPAR_GETHDS( NAMECODE, STRING(1:SLEN), ACCESS,
     :                               LOC, STATUS )

               ELSE IF ( ( ACTION .EQ. LEX__STRING )
     :              .OR. ( ACTION .EQ. LEX__INTEGER )
     :              .OR. ( ACTION .EQ. LEX__REAL )
     :              .OR. ( ACTION .EQ. LEX__DOUBLE )
     :              .OR. ( ACTION .EQ. LEX__LOGICAL ) ) THEN

*           Primitive item (String, Integer, Real, Double Precision, Logical)
*           All these can be handled by the same code as SUBPAR_PUT0C
*           will do any necessary conversion
                     CALL SUBPAR_STORE0( NAMECODE, ACTION, STRING, SLEN,
     :                                   LOC, STATUS )

               ELSE IF ( ACTION .EQ. LEX__STARR ) THEN
*   Array
                  CALL SUBPAR_ARRAY( NAMECODE, COMMAND(1:ENDLINE), LOC,
     :                              STATUS )

*   At this stage, any ACTION other than 0 is an error
*   ACTION 0 may have been set by MIN/MAX recognised earlier
               ELSE IF ( ACTION .NE. 0 ) THEN

                  STATUS = SUBPAR__CMDSYER
                  CALL EMS_SETC( 'STRING', STRING(1:SLEN) )
                  CALL EMS_REP( 'SUP_HDSIN3',
     :                   'SUBPAR: Input line syntax error /^STRING/'
     :                   , STATUS )

               END IF


*   Change to a SUBPAR error code
               IF ( STATUS .EQ. LEX__INVCHAR ) THEN
                  STATUS = SUBPAR__CMDSYER
                  CALL EMS_REP( 'SUP_HDSIN4',
     :           'SUBPAR: Syntax error in parsing given parameter '//
     :           'value', STATUS )
               END IF

            END IF

         END IF

      END IF

*   Release the error context
      CALL EMS_RLSE

      END
