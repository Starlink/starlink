      SUBROUTINE SUBPAR_CMDLINE(ACTCODE, CONTEXT, CMDLINE, STATUS)
*+
*  Name:
*     SUBPAR_CMDLINE

*  Purpose:
*     parse command line.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_CMDLINE(ACTCODE,CONTEXT,CMDLINE,STATUS)

*  Description:
*     Parse an ADAM command line and enter the parameter values
*     into the parameter system

*  Arguments:
*     ACTCODE = INTEGER (given)
*        Name code for action being obeyed
*     CONTEXT = INTEGER (given)
*        Context (OBEY or CANCEL)
*     CMDLINE = CHARACTER*(*) (given)
*        The command line to be parsed
*     STATUS = INTEGER

*  Algorithm:
*     A list of parameter namecodes is constructed based on the needs list
*     or the program section of the parameter storage.
*     Pre-parse to replace directory specs in [] to ones in <>
*     The command line is parsed by calling the LEX parser which returns
*     an action code for each item found in the string, together with a
*     string representing that item. This action code is used to determine
*     how the value of the corresponding parameter is to be set.
*     If a start array ([) is found, SUBPAR_ARRAY is used to parse the array
*     and set the parameter value.

*  Copyright:
*     Copyright (C) 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 1999, 2000, 2001, 2004 Central Laboratory of the Research Councils.
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
*     JAB: J A  Bailey  (AAO)
*     AJC: A J Chipperfield (STARLINK)
*     TJF: T J Farrell (AAO)
*     PWD: P W Draper (STARLINK)
*     DSB: D S Berry (EAO)

*  History:
*     08-JAN-1987 (JAB):
*        Original version
*     12-JAN-1987 (JAB):
*        Add Array handling
*     15-JAN-1987 (JAB):
*        Allow a primitive type to be written
*        into a parameter which has a non primitive type
*        specified in the interface file
*     15-JAN-1987 (JAB):
*        Use SUBPAR_PUT to write values, so works for
*        N-D arrays.
*     16-JAN-1987 (JAB):
*        Call SUBPAR_CANCL after a conversion failure
*     19-JAN-1987 (JAB):
*        Revise error codes
*     22-JAN-1987 (JAB):
*        Add CONTEXT - so works for OBEY or CANCEL
*     30-JAN-1987 (JAB):
*        Fix Bug in array handling - don't call
*        LEX_CMDLINE at end if LEVEL less than 1
*     30-JAN-1987 (JAB):
*        Allow lower case logical constants
*     11-MAY-1987 (JAB):
*        Exit from loop on ACTION = 98 (ELINE)
*     28-MAY-1987 (JAB):
*        Add handling of ACCEPT, PROMPT and RESET
*     29-MAY-1987: (BDK)
*        Change include name to LEX_ERR
*     03-JUL-1988 (JAB):
*        Allow directory specs in [ ] to be distinguished
*        from arrays
*     04-JUL-1988 (AJC):
*        Allow 'no' as well as 'NO'keyword
*     07-DEC-1988 (AJC):
*        Allow for ] followed by ] or [ in check for directory spec.
*        Work with 'used' length of COMMAND
*        Insert spaceCR to prevent LEX__ELINE on last parameter
*        Use LEX_PAR symbolic constants
*     20-DEC-1988 (AJC):
*        Re-write test for logical constant to allow mixed
*        case
*     03-MAY-1989 (AJC):
*        Correct for case of unallocated parameter position
*     06-APR-1990 (AJC):
*        Increase command line size to 444 characters
*     15-JUN-1990 (AJC):
*        prevent over running the array bounds of NAMECODE
*        improve check for logical keywords
*        re-structure test for NEXTPAR=0 - report error on
*        using unallocated position
*     12-OCT-1990 (AJC):
*        Correct failure to set PARSET on primitive internal
*        parameters
*     29-OCT-1990 (AJC):
*        Increase allowed number of command line parameters
*        (i.e. POSITION fields) from 32.
*     15-FEB-1991: (AJC/TJF)
*        If keywords are found, ensure that the assocated parameter
*        is needed by this command.  Otherwise, we may set the
*        wrong parameter. (RLVAD::AJC after AAO::TJF)
*     22-FEB-1991 (AJC):
*        correct above mod - if there is no NEEDS list accept all
*        task parameters
*     23-SEP-1991 (AJC):
*        correctly handle error reports
*     10-JUL-1992 (AJC):
*        skip out if empty command line - avoids potential string bounds fault
*        set PARSET after 'internal' arrays
*        SPAG and FORCHECK mods
*     20-JUL-1992 (AJC):
*        Move DATA statement
*     16-NOV-1992 (AJC):
*        MIN/MAX system
*        Command line NULL (!)
*        Strip out array handling to SUBPAR_ARRAY
*        Use SUBPAR_DIRFX to change [] to <> for directories
*     10-MAR-1993 (AJC):
*        Use CHAR(92) for portability of backslash character
*        Add DAT_PAR for SUBPAR_CMN
*     11-MAR-1993 (AJC):
*        Add KEYWORD=\ feature
*     16-MAR-1993 (AJC):
*        Changed name of SUBPAR_ACCPR/1 to SUBAPR_ACCPT/1
*      7-MAY-1993 (AJC):
*        Fix up D exponents so HDS can handle them
*      4-JUN-1993 (AJC):
*        Assume MIN/MAX is name for non-primitives
*      6-SEP-1993 (AJC):
*        Remove unused SUBPAR_CTYPE declaration
*      5-AUG-1994 (AJC):
*        Cancel before setting MIN, MAX or NULL state
*      5-SEP-1994 (AJC):
*        Extra arguments on SUBPAR_FINDKEY call
*        and call FINDKEY with upper case string.
*        and use FINDKEY to check special keywords also
*      6_DEC-1994 (AJC):
*        Correct a bug disallowing logical keywords beginning NO
*     20-DEC-1995 (AJC):
*        Create Parameter file component of appropriate size for CHAR
*        (min 132),
*        Allow input string up to 444 chars (was 200).
*     16-DEC-1998 (AJC):
*        Remember character value if MIN/MAX on command line.
*        Use SUBPAR_STORE0 to store primitve scalar values
*     14-JAN-1999 (AJC):
*        Prevent KEYWORDS allowing last positional parameter being
*        overwritten if too many positional params given.
*     18-MAY-1999 (AJC):
*        Remove unused variables
*      1-JUL-1999 (AJC):
*        Correct position after keyword (bug introduced 14-JAN).
*     21-MAR-2000 (AJC):
*        Remove the trapping of directories in [] - VMS requirement
*        causes problems with GRP
*     20-DEC-2001 (AJC):
*        Set PARSET on setting INTERNAL param with primitive value
*         (thus causing NEXTPAR increment)
*     30-SEP-2004 (PWD):
*        Stop USTRING being indexed by greater than SUBPAR__NAMELEN.
*        It is often indexed by SLEN, which can be up to MCLENGTH.
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
      INCLUDE 'ADAM_DEFNS'
      INCLUDE 'LEX_ERR'
      INCLUDE 'LEX_PAR'
      INCLUDE 'MESSYS_PAR'

*  Arguments Given:
      INTEGER ACTCODE
      INTEGER CONTEXT
      CHARACTER*(*) CMDLINE

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  External routines :
      INTEGER CHR_LEN            ! Used length of string

*  Local Constants:
      INTEGER MAXPARS            ! maximum number of command line parame
      PARAMETER (MAXPARS=50)
      CHARACTER*15 POSTYPES(8)   ! Possible primitive data types

*  Local Variables:
      CHARACTER*(MESSYS__VAL_LEN+2) COMMAND  ! Command line with spaceCR added
      INTEGER ENDLINE             ! Position of end of command line
      INTEGER ACTION              ! Action code from parser
      INTEGER NUMPARS             ! Number of parameters
      CHARACTER*(MESSYS__VAL_LEN) STRING ! Parameter string from parser
      INTEGER SLEN                ! length of above
      INTEGER ISLEN               ! SLEN index into USTRING.
      CHARACTER*(SUBPAR__NAMELEN) USTRING  ! upper case STRING
      INTEGER NAMECODE(MAXPARS)   ! Namecodes of parameters for each pos
      INTEGER I, J                ! Loop counters
      INTEGER NEXTPAR             ! Namecode of next parameter to be set
      INTEGER NEXTINDEX           ! Index in namecode array of next para
      INTEGER KEYSTART            ! Index possible start of keyword
      INTEGER KEYCODE             ! Namecode of parameter found by keywo
      INTEGER TYPE                ! type code for parameter
      CHARACTER*(DAT__SZLOC) LOC  ! HDS Locator to parameter storage
      LOGICAL PARSET              ! True if parameter value set
      INTEGER STAT                ! Temporary status
                                  ! type for array
      LOGICAL LVALUE              ! Conversion of logical constant
      LOGICAL NONEEDS             ! True if there is a NEEDS list
      LOGICAL SPECIAL             ! Special keywor flag for FFINDKEY

*  Local Data:
      DATA POSTYPES/'_CHAR*', '_REAL', '_DOUBLE', '_INTEGER',
     :     '_LOGICAL',  ' ', ' ', '_INT64' /
*.

      IF ( STATUS.NE.SAI__OK ) RETURN

*  Set error context
      CALL EMS_MARK

*  Find length of command line (+2 for termination)
      ENDLINE = MIN(MESSYS__VAL_LEN, CHR_LEN(CMDLINE)) + 2

*  If there is anything on the command line, process it
      IF ( ENDLINE.GT.2 ) THEN

*     Add a CR at end of command line
         COMMAND = CMDLINE(1:ENDLINE-2)//' '//CHAR(13)

*  Fill the array of parameter namecodes - If the context is CANCEL
*  then the positions are based on the CANCEL needs list for the action
*  Otherwise they are taken from the OBEY needs list if present. If
*  this is not present they come from the POSITION specifiers in the
*  interface file.
*  NAMECODE(1) contains the parameter namecode for the parameter allocated
*  position 1 etc.

*     Ensure NONEEDS is true unless set false.
         NONEEDS = .TRUE.

         IF ( CONTEXT.EQ.CANCEL ) THEN
            IF ( NEEDCAN(1,ACTCODE).GT.0 ) THEN
*           Parameter positions are in CANCEL NEEDS list
*           Check there are not too many
               IF ( (NEEDCAN(2,ACTCODE)-NEEDCAN(1,ACTCODE)+1)
     :              .LE.MAXPARS ) THEN
*              Flag that there is a NEEDS list
                  NONEEDS = .FALSE.
*              Set up NAMECODE
                  NUMPARS = 0
                  DO J = NEEDCAN(1, ACTCODE), NEEDCAN(2, ACTCODE)
                     NUMPARS = NUMPARS + 1
                     NAMECODE(NUMPARS) = NEEDPAR(J)
                  END DO
               ELSE
*              Maximum number of positional parameters exceeded
                  STATUS = SUBPAR__XMXPOS
                  CALL EMS_SETI('MAXPARS', MAXPARS)
                  CALL EMS_REP('SUP_CMDLINE1',
     :             'SUBPAR: Maximum number (^MAXPARS) of positional '
     :             //'parameters exceeded', STATUS)
               END IF

            END IF

         ELSE IF ( CONTEXT.EQ.OBEY ) THEN
            IF ( NEEDOB(1,ACTCODE).LE.0 ) THEN

*           Parameter positions from program section of parameter store
               I = 0
               NUMPARS = 0
               DO J = PROGADD(1, PROGNUM), PROGADD(2, PROGNUM)
                  I = I + 1
                  IF ( I.LE.MAXPARS ) THEN
*                 Provided we are not outside NAMECODE,
*                 get the highest allocated position number
                     IF ( PARPOS(J).NE.0 ) NUMPARS = I
*                    and copy PARPOS to NAMECODE even if zero (to overwrite
*                    any previously set values).
                     NAMECODE(I) = PARPOS(J)

                  ELSE IF ( PARPOS(J).NE.0 ) THEN
*                 We are outside NAMECODE and need to allocate another
*                 position. This is an error.
                     STATUS = SUBPAR__XMXPOS
                     CALL EMS_SETI('MAXPARS', MAXPARS)
                     CALL EMS_REP('SUP_CMDLINE2',
     :               'SUBPAR: Maximum number (^MAXPARS) of ' //
     :               'positional parameters exceeded', STATUS)
                  END IF
               END DO

*   Parameter positions are in OBEY NEEDS list
*   Check there are not too many
            ELSE IF ( (NEEDOB(2,ACTCODE)-NEEDOB(1,ACTCODE)+1)
     :                .LE.MAXPARS ) THEN
*           Flag that there is a NEEDS list
               NONEEDS = .FALSE.
*           Set up NAMECODE
               NUMPARS = 0
               DO J = NEEDOB(1, ACTCODE), NEEDOB(2, ACTCODE)
                  NUMPARS = NUMPARS + 1
                  NAMECODE(NUMPARS) = NEEDPAR(J)
               END DO
            ELSE
               STATUS = SUBPAR__XMXPOS
               CALL EMS_SETI('MAXPARS', MAXPARS)
               CALL EMS_REP('SUP_CMDLINE3',
     :          'SUBPAR: Maximum number (^MAXPARS) of positional ' //
     :          'parameters exceeded', STATUS)
            END IF
         END IF

*  Initialise pointers and flags
         IF ( NUMPARS.GT.0 ) THEN
            NEXTPAR = NAMECODE(1)
         ELSE
            NEXTPAR = 0
         END IF
         NEXTINDEX = 2
         PARSET = .FALSE.

*  Initiate parse

         CALL LEX_CMDLINE(.TRUE., COMMAND(1:ENDLINE), ACTION, STRING,
     :                    SLEN, STATUS)
         DO WHILE ((STATUS.EQ.SAI__OK) .AND. (ACTION.LT.LEX__ELINE))

*  First deal with the ambiguous action codes
*        Get upper case copy of STRING if we need to test for Keywords etc.
            IF ( ( ACTION .EQ. LEX__KAMBIG )
     :      .OR. ( ACTION .EQ. LEX__AMBIG ) ) THEN
                USTRING = STRING(1:SUBPAR__NAMELEN)
                CALL CHR_UCASE ( USTRING )

*        Otherwise set USTRING to blanks
            ELSE
                USTRING = ' '
            END IF

            IF ( ACTION.EQ.LEX__KAMBIG ) THEN

*           Special or Logical Keyword, String, Name or Logical Constant
*           See if it is one of the LOGICAL keywords
                SPECIAL = .TRUE.
                KEYSTART = 1
                ISLEN = MIN( SLEN, SUBPAR__NAMELEN )
                CALL SUBPAR_FINDKEY(USTRING(KEYSTART:ISLEN),
     :                              SPECIAL, .TRUE., KEYCODE, STATUS)

                IF ( STATUS .NE. SAI__OK ) THEN
*             Wasn't a keyword - check for NOkeyword
                    IF ( (SLEN.GE.3)
     :              .AND. (USTRING(1:2).EQ.'NO') ) THEN
                       CALL EMS_ANNUL( STATUS )
                       KEYSTART = 3
*                  It can't be a special keyword
                       SPECIAL = .FALSE.
*                  See if remainder is a LOGICAL keyword
                       CALL SUBPAR_FINDKEY(USTRING(KEYSTART:ISLEN),
     :                                     SPECIAL, .TRUE., KEYCODE,
     :                                     STATUS)
                    END IF

                END IF

*              If failed to find keyword
                  IF ( STATUS.NE.SAI__OK ) THEN
*                 Must be String, Name, MIN/MAX or Logical constant
                     CALL EMS_ANNUL(STATUS)
                     ACTION = LEX__AMBIG

*              Check for the special keywords
                  ELSE IF ( KEYCODE .LT. 0 ) THEN
                     IF ( KEYCODE .EQ. -1 ) THEN
                        CALL SUBPAR_ACCPT(STATUS)
                        ACTION = 0

                     ELSE IF ( KEYCODE .EQ. -2 ) THEN
                        CALL SUBPAR_FPROMPT(STATUS)
                        ACTION = 0

                     ELSE IF ( KEYCODE .EQ. -3 ) THEN
                        CALL SUBPAR_RESET(STATUS)
                        ACTION = 0

                     END IF

*              Else it was the keyword of a LOGICAL parameter
                  ELSE
*                 Check if the parameter is required for this action
                     J = NEXTINDEX - 1
*                 Ensure the loop is obeyed at least once
                     IF ( J.GT.NUMPARS ) J = NUMPARS
                     DO WHILE (J.LE.NUMPARS)
                        IF ( (NAMECODE(J).EQ.KEYCODE) .OR. (NONEEDS) )
     :                        THEN
*                       The parameter is one for this action
*                       Set parameter pointers, ACTION code and STRING
*                       appropriately.
                           NEXTPAR = KEYCODE
                           NEXTINDEX = NEXTINDEX - 1
                           ACTION = LEX__LOGICAL
                           IF ( KEYSTART.EQ.1 ) THEN
*                          Wasn't 'NO' form.
                              STRING = 'T'
                           ELSE
*                          Was 'NO' form
                              STRING = 'F'
                           END IF
                           SLEN = 1
*                       Force end of loop
                           J = NUMPARS
                        END IF
*                    Try next required parameter until found or finished
                        J = J + 1
                     END DO

*                 If it was a logical keyword but not one for this action
*                 it must be a string, name or logical constant.
                     IF ( ACTION.NE.LEX__LOGICAL ) ACTION = LEX__AMBIG

                  END IF

            END IF


            IF ( (ACTION.EQ.LEX__AMBIG) .AND. (NEXTPAR.NE.0) ) THEN
*           Resolve MIN/MAX, !, \, String or Name or Logical Constant
               TYPE = MOD(PARTYPE(NEXTPAR), 10)

*           Check for MIN or MAX
*           Set parameter state if type allows so that the min or max value
*           will be selected WHEN THE VALUE IS REQUESTED.
*           Also set PARVALS for to the actual string for _CHAR and LITERAL
*           (So it can be restored with correct case if there is no MIN/MAX
*           set when the request comes).
*           Cancel any existing value first.
               ISLEN = MIN( SLEN, SUBPAR__NAMELEN )
               IF ( (USTRING(1:ISLEN) .EQ. 'MAX') .OR.
     :              (USTRING(1:ISLEN) .EQ. 'MIN')) THEN
                  IF ( ( TYPE .EQ. SUBPAR__REAL ) .OR.
     :                 ( TYPE .EQ. SUBPAR__INTEGER ) .OR.
     :                 ( TYPE .EQ. SUBPAR__DOUBLE ) .OR.
     :                 ( TYPE .EQ. SUBPAR__CHAR ) ) THEN
                     CALL SUBPAR_CANCL( NEXTPAR, STATUS )
                     IF (USTRING(1:ISLEN) .EQ. 'MAX') THEN
                        PARSTATE( NEXTPAR ) = SUBPAR__MAX
                        IF ( TYPE .EQ. SUBPAR__CHAR )
     :                     PARVALS( NEXTPAR ) = STRING
                     ELSE
                        PARSTATE( NEXTPAR ) = SUBPAR__MIN
                        IF ( TYPE .EQ. SUBPAR__CHAR )
     :                     PARVALS( NEXTPAR ) = STRING
                     ENDIF
*                 No further action is required
                     ACTION = 0
                     PARSET = .TRUE.

*              Otherwise assume MIN/MAX is a name
*              (Types LOGICAL or non-primitive)
                  ELSE
                     ACTION = LEX__NAME

                  END IF

*           Check for NULL specifier
               ELSE IF ( STRING(1:SLEN) .EQ. '!' ) THEN
*              Set state NULL - cancel any existing value first
                  CALL SUBPAR_CANCL( NEXTPAR, STATUS )
                  PARSTATE( NEXTPAR ) = SUBPAR__NULL
*              and flag parameter set and no further action
                  PARSET = .TRUE.
                  ACTION = 0

*           Check for \ (ACCEPT) following KEYWORD=
               ELSE IF (
     :            (USTRING(1:MIN(SLEN,SUBPAR__NAMELEN)) .EQ. 'ACCEPT')
     :            .OR.
     :            (USTRING(1:MIN(SLEN,SUBPAR__NAMELEN )) .EQ. CHAR(92))
     :         ) THEN
                  CALL SUBPAR_ACCPT1( NEXTPAR, STATUS )
                  PARSET = .TRUE.
                  ACTION = 0

*           Check for special case of CHARACTER
               ELSE IF ( TYPE.EQ.SUBPAR__CHAR ) THEN
*              Interpret as literal string
                  ACTION = LEX__STRING

               ELSE IF ( (TYPE.EQ.SUBPAR__LOGICAL) .OR.
     :                   (TYPE.EQ.SUBPAR__NOTYPE) ) THEN
*              Attempt to convert the string to logical
                  STAT = SAI__OK
                  CALL CHR_CTOL(STRING(1:SLEN), LVALUE, STAT)
                  IF ( STAT.EQ.SAI__OK ) THEN
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

            ELSE IF ( (ACTION.EQ.LEX__AMBIG) .AND. (NEXTPAR.EQ.0) ) THEN
*           Set LEX__NAME. Anything which will flag the attempt to use
*           an unallocated position would do.
               ACTION = LEX__NAME

            END IF


*
*   Now, having resolved ambiguities, handle the required action.
*   First check for those ACTIONS which are allowed even if the command
*   line position is unallocated.

            IF ( ACTION.EQ.LEX__KEYWORD ) THEN

*   Keyword - If it matches a keyword of the command, set that
*             parameter as the next parameter to be written
*             and decrement index so that the next positional
*             parameter will be used after that
*             First get an upper case copy of STRING
               USTRING = STRING(1:SUBPAR__NAMELEN)
               CALL CHR_UCASE ( USTRING )
               ISLEN = MIN( SLEN, SUBPAR__NAMELEN )
               CALL SUBPAR_FINDKEY(USTRING(1:ISLEN), .FALSE., .FALSE.,
     :          KEYCODE, STATUS)
               IF ( STATUS .EQ. SAI__OK ) THEN
                  NEXTPAR = KEYCODE
                  NEXTINDEX = NEXTINDEX - 1
               ELSE
                  STATUS = SUBPAR__CMDSYER

               END IF


            ELSE IF ( ACTION.EQ.0 ) THEN
*  ACTION = 0 signifies "no action" required here (it was a special keyword).
*           Get the next parameter.
               CONTINUE

*  All other possibilities require that this command line position is
*  allocated to a parameter. If it is not, report an error.
            ELSE IF ( NEXTPAR.EQ.0 ) THEN
*  Position is unallocated
               STATUS = SUBPAR__PNOTAL
               CALL EMS_SETC('STRING', STRING(1:SLEN))
               CALL EMS_REP('SUP_CMDLINE6',
     :          'SUBPAR: Attempt to use ''positional'' parameter value '
     :          //'(^STRING) in an unallocated position', STATUS)

            ELSE IF ( ACTION.EQ.LEX__NAME ) THEN
*  Name (e.g. HDS or device name)
               CALL SUBPAR_PUTNAME(NEXTPAR, STRING(1:SLEN), STATUS)
               PARSET = .TRUE.

*   Primitive item (String, Integer, Real, Double Precision, Logical)
            ELSE IF ( (ACTION.EQ.LEX__STRING) .OR.
     :                (ACTION.EQ.LEX__INTEGER) .OR.
     :                (ACTION.EQ.LEX__REAL) .OR.
     :                (ACTION.EQ.LEX__DOUBLE) .OR.
     :                (ACTION.EQ.LEX__LOGICAL) ) THEN

*           All these can be handled by the same code as SUBPAR_PUT0C
*           will do any necessary conversion

*           If parameter is internal cancel any previous HDS association
*           and store the value

               IF ( PARVPATH(1,NEXTPAR).EQ.SUBPAR__INTERNAL ) THEN
                  CALL SUBPAR_CANCL(NEXTPAR, STATUS)
                  CALL SUBPAR_PUT0C(NEXTPAR, STRING(1:SLEN), STATUS)
                  PARSET = .TRUE.

*           If not internal create an HDS component of the same type as
*           the parameter and put the value in it. If the parameter
*           has a primitive type as defined in the interface file
*           use that type - Otherwise use the type derived from the
*           parsing of the string
               ELSE

                  CALL SUBPAR_STORE0( NEXTPAR, ACTION, STRING, SLEN,
     :                                LOC, STATUS )
                  CALL DAT_ANNUL( LOC, STATUS )
                  PARSET = .TRUE.
               END IF

            ELSE IF ( ACTION.EQ.LEX__STARR ) THEN
*     Parse an array on the command line and set parameter accordingly
               CALL SUBPAR_ARRAY( NEXTPAR, COMMAND(1:ENDLINE), LOC,
     :                           STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL SUBPAR_CANCL( NEXTPAR, STATUS )
               ELSE
*           The returned locator is not required
                  CALL DAT_ANNUL( LOC, STATUS )
               ENDIF
               PARSET = .TRUE.

            ELSE
*     At this stage, any other ACTION is an error
               STATUS = SUBPAR__CMDSYER
               CALL EMS_SETC('STRING', STRING(1:SLEN))
               CALL EMS_REP('SUP_CMDLINE8',
     :                     'SUBPAR: Command line syntax error /^STRING/'
     :                     , STATUS)

            END IF

*   If a parameter was set find the next one
            IF ( PARSET ) THEN
               IF ( NEXTINDEX .LE. NUMPARS ) THEN
                  NEXTPAR = NAMECODE(NEXTINDEX)
               ELSE
                  NEXTPAR = 0
               END IF
               NEXTINDEX = NEXTINDEX + 1
               PARSET = .FALSE.
            END IF

            CALL LEX_CMDLINE(.FALSE., COMMAND(1:ENDLINE), ACTION,
     :                       STRING, SLEN, STATUS)

*  End of command line parsing loop
         END DO


*  Check for known LEX errors
         IF ( STATUS.EQ.LEX__ENDPARSE ) THEN
            CALL EMS_ANNUL(STATUS)
         ELSE IF ( STATUS.EQ.LEX__INVCHAR ) THEN
            STATUS = SUBPAR__CMDSYER
            CALL EMS_REP('SUP_CMDLINE9',
     :                   'SUBPAR: Syntax error in parsing command line',
     :                   STATUS)
         END IF

      END IF

*  Release error context
      CALL EMS_RLSE

      END
