      SUBROUTINE SUBPAR_GETNAME ( NAMECODE, STRUCTNAME, STATUS )
*+
*  Name:
*     SUBPAR_GETNAME

*  Purpose:
*     Get a valid name as a parameter value.
*     The value may be supplied as a forced name (@name) or as a string value
*     possibly quoted. The @ or quotes will be stripped from the value
*     returned.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_GETNAME ( NAMECODE, STRUCTNAME, STATUS )

*  Description:
*     If a name or character string has not been preset as the parameter value,
*     the VPATH for the indicated parameter is followed to obtain a name
*     as the parameter value.
*
*     If the preset value was a character string, it is re-defined as a name.
*
*     The name is 'checked' by SUBPAR_CHECKNAME but in fact this allows
*     anything as a name and strips leading '@' or quotes from the name
*     obtained.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to the parameter
*     STRUCTNAME=CHARACTER*(*) (returned)
*        the structure name
*     STATUS=INTEGER

*  Algorithm:
*     If the parameter has the name of a data structure already, return
*     it. Otherwise, check in turn the data sources suggested by
*     the VPATH until one of them gives a valid result, and store the
*     name with the parameter.
*     If the vpath is overridden, by CANCEL, FPROMPT etc., prompt the
*     user. Prompt up to five times.
*     If a valid name is not obtained, return status PAR__NULL.

*  Implementation Deficiencies:
*     The setting of PARERRMESS should be improved.

*  Copyright:
*     Copyright (C) 1984, 1985, 1987, 1988, 1990, 1991, 1992, 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 1996, 1998 Central Laboratory of the Research Councils.
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
*     02-OCT-1984 (BDK):
*        Original
*     05-FEB-1985 (BDK):
*        Copy the name into the parameter store if it wasn't
*        there already
*     08-MAY-1987 (BDK):
*        put error message if parameter ACTIVE but with bad
*        value - eg from faulty command-line
*     08-MAY-1987 (BDK):
*        check for NULL default
*     28-MAY-1987 (BDK):
*        handle where VPATH has been overridden
*     10-JUN-1987 (BDK):
*        trap bad status on overridden VPATH
*     10-JUN-1987 (BDK):
*        allow RESET parameters to follow VPATH
*     30-JUL-1987 (BDK):
*        force prompting in command-line error
*     18-AUG-1987 (BDK):
*        add VPATH 'current'
*     30-MAY-1988 (BDK):
*        don't PUTNAME if the parameter is ACTIVE
*     10-JUL-1988 (AJC):
*        Use SUBPAR_CURSAV to save 'current' value
*        don't follow vpath in CANCELLED state
*        let RESET ignore 'current' on vpath also
*     11-SEP-1990 (AJC):
*        Handle vpath overridden inside the loop.
*        Allow 5 re-prompts on bad values.
*     18-JUL-1991 (AJC):
*        Use EMS not LIB$GETMSG
*     05-AUG-1991 (AJC):
*        Re-organize error reporting
*        add message on prompt count exceeded
*     20-SEP-1991 (AJC):
*        Correctly annul errors along vpath
*     24-SEP-1991 (AJC):
*        Prefix messages with 'SUBPAR:'
*     24-APR-1992 (AJC):
*        Release error context
*     14-MAY-1992 (AJC):
*        Report on NULL default taken
*        USE SUBPAR_EFLSH to flush errors
*        Check default/dynamic set before testing type
*     22-JUN-1992 (AJC):
*        Report if numeric preset value - character string is OK.
*        Also accept character string dynamic default
*        Re-define type if preset value is a string
*        (The above mods allow a preset string or string dynamic default
*        to be used as the name. A mod to PARSECON_CONVERT allows a string
*        static default to be set.)
*     17-JUL-1992 (AJC):
*        Correct failure to see DEFAULT NULL because of test for
*        PARDEF(1,NAMECODE) set.
*      8-SEP-1992 (AJC):
*        Annul temporary locator to parameter file entry
*      9-OCT-1992 (AJC):
*        Let RESET affect VPATH.
*     16-NOV-1992 (AJC):
*        Make ACCPR not override VPATH
*     16-MAR-1993 (AJC):
*        Allow for new states ACCEPT, RESACCPR
*        Remove ACTIVE state from possibilities in vpath search
*        Remove spurious test for NULL and ABORT in CURRENT section
*      9-AUG-1993 (AJC):
*        INCLUDE SUBPAR_PARERR not PAR_ERR
*        Remove INCLUDE DAT_ERR
*      4-AUG-1994 (AJC):
*        Also terminate on PAR__NOUSR from prompt.
*      5-SEP-1996 (AJC):
*        Allow string as default
*      5-FEB-1998 (AJC):
*        Set given value of STRUCTURE in SUBPAR_CHECKNAME call:
*        TRUE existing value is already a name (preserves quotes
*        during unquoting process); FALSE otherwise

*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PARERR'
      INCLUDE 'SUBPAR_ERR'
      INCLUDE 'SUBPAR_PAR'

*  Arguments Given:
      INTEGER NAMECODE                ! pointer to parameter

*  Arguments Returned:
      CHARACTER*(*) STRUCTNAME        ! name of HDS structure

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  Local Variables:
      INTEGER J                       ! point on VPATH

      LOGICAL FINISHED                ! flag for VPATH search complete

      LOGICAL STRUCTURE               ! .TRUE. => string has syntax of a
                                      ! structure name

      CHARACTER*(DAT__SZLOC) LOC      ! Locator to current value

      CHARACTER*(DAT__SZTYP) TYPE    ! Type of parameter file object
*.


      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Set new error context
*
      CALL EMS_MARK
*
*   Check that the STATE of the parameter allows a value to be got
*
      IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__NULL ) THEN

         STATUS = PAR__NULL

      ELSE
*
*   Start the search with the current parameter value
*
         FINISHED = .FALSE.
         STRUCTNAME = ' '
         IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACTIVE ) THEN

            IF ( PARTYPE(NAMECODE) .GE. 20 ) THEN

*           Get the stored name or string
               STRUCTNAME = PARVALS(NAMECODE)

            ELSE IF ( PARTYPE(NAMECODE) .GE. 10 ) THEN
               CALL DAT_FIND ( EXTLOC, PARNAMES(NAMECODE), LOC, STATUS )
               CALL DAT_TYPE ( LOC, TYPE, STATUS )
               IF ( TYPE(1:6) .EQ. '_CHAR*' ) THEN
*              The object is character assume it contains the name required
                  CALL DAT_GET0C ( LOC, STRUCTNAME, STATUS )
               ENDIF
               CALL DAT_ANNUL( LOC, STATUS )
            ENDIF

            IF ( STATUS .EQ. SAI__OK ) THEN
*           Check and tidy the name
               STRUCTURE = .TRUE.
               CALL SUBPAR_CHECKNAME ( STRUCTNAME, STRUCTURE,
     :           STATUS )
               IF ( STRUCTURE ) FINISHED = .TRUE.

            ELSE
               STRUCTNAME = ' '

            ENDIF

            IF ( STRUCTNAME .EQ. ' ' ) THEN
*           Report error message to be displayed before any re-prompt
*           This condition probably has arisen because of a typing
*           error on the command-line, so force prompting rather than
*           follow the VPATH.
               IF ( STATUS .EQ. SAI__OK ) STATUS = SUBPAR__NAMIN
               CALL EMS_SETC ( 'NAME', PARKEY(NAMECODE) )
               CALL EMS_REP ( 'SUP_GETNAME1',
     :         'SUBPAR: Bad preset value for parameter ^NAME', STATUS )
               CALL EMS_REP ( 'SUP_GETNAME1',
     :         'SUBPAR: A name is required', STATUS )
               PARSTATE(NAMECODE) = SUBPAR__FPROMPT
            ENDIF

         ENDIF
*
*      Try to get the name from elsewhere, following the VPATH
*      Possible steps on the path are
*      Path exhausted => PAR__NULL ( should never happen. error?)
*      NOPROMPT       => PAR__NULL
*      PROMPT         => prompt
*      NOPATH         => prompt
*      DEFAULT        => static default
*      DYNAMIC        => dynamic default - accept character type if set
*      GLOBAL         => get from ASSOCIATION structure
*      INTERNAL       => program 'private' storage
*      CURRENT        => program 'private' storage
*
         J = 0

         DO WHILE ( .NOT. FINISHED )

            J = J + 1

*         If status is bad, flush the messages
            IF ( STATUS .NE. SAI__OK ) THEN
                CALL SUBPAR_EFLSH( STATUS )
            ENDIF
*
*      Handle cases where VPATH has been overridden or VPATH specifies
*      prompt or has expired.
            IF ( ( J .LE. 10 ) .AND.

*           VPATH overridden by a state forcing prompt
     :        (( PARSTATE(NAMECODE) .EQ. SUBPAR__CANCEL ) .OR.
     :         ( PARSTATE(NAMECODE) .EQ. SUBPAR__FPROMPT ) .OR.
     :         ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACCPR ) .OR.
     :         ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESPROM ) .OR.
     :         ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESACCPR ) .OR.

*           or VPATH requires prompt
     :         ( PARVPATH(J,NAMECODE) .EQ. SUBPAR__NOPATH ) .OR.
     :         ( PARVPATH(J,NAMECODE) .EQ. SUBPAR__PROMPT ))) THEN

*            Use count 6 to 10 for prompt repeats
               IF ( J .LE. 5 ) J = 6

               CALL SUBPAR_INPUT ( NAMECODE, STRUCTNAME, STATUS )
               STRUCTURE = .FALSE.
               CALL SUBPAR_CHECKNAME ( STRUCTNAME, STRUCTURE, STATUS )

               IF ( ( STATUS .EQ. PAR__NULL ) .OR.
     :              ( STATUS .EQ. PAR__ABORT ) .OR.
     :              ( STATUS .EQ. PAR__NOUSR ) ) THEN
*               Requested to abandon the attempt.
                  FINISHED = .TRUE.

               ELSE IF ( ( STATUS .EQ. SAI__OK )
     :             .AND. ( .NOT. STRUCTURE ) ) THEN
*               Value obtained but it wasn't a valid name -
*               report error message to be displayed with any re-prompt.
                  CALL EMS_SETC ( 'NAME', PARKEY(NAMECODE) )
                  CALL EMS_SETC ( 'STRUC', STRUCTNAME )
                  CALL EMS_REP ( 'SUP_GETNAME2',
     :            'SUBPAR: Invalid name ^STRUC given for '//
     :            'parameter ^NAME', STATUS )
*               Set STATE FPROMPT to force reprompt
                  PARSTATE( NAMECODE) = SUBPAR__FPROMPT

               ELSE
*               All OK
                  FINISHED = .TRUE.
               ENDIF

            ELSEIF ( ( J .GT. 5 ) .OR. ( PARVPATH(J,NAMECODE) .EQ.
     :        SUBPAR__NOPROMPT) ) THEN
*            Give up trying to get a value. Set PAR__NULL and reset
*            error message.
               FINISHED = .TRUE.
               PARERRMESS = ' '
               STATUS = PAR__NULL
*            If due to maximum re-prompt failure, report it
               IF ( J .GT. 10 ) THEN
                  CALL EMS_SETC ( 'NAME', PARKEY(NAMECODE) )
                  CALL EMS_REP ( 'SUP_GETNAME2',
     :            'SUBPAR: 5 prompts failed to get a good value for '//
     :            'parameter ^NAME - NULL assumed', STATUS )

*            Else if NOPROMPT on vpath
               ELSE
                  CALL EMS_SETC ( 'NAME', PARKEY(NAMECODE) )
                  CALL EMS_REP ( 'SUP_GETNAME3',
     :            'SUBPAR: NOPROMPT on vpath for parameter ^NAME - '//
     :            'NULL assumed', STATUS )

               ENDIF

            ELSE IF ( PARVPATH(J,NAMECODE) .EQ. SUBPAR__INTERNAL )
     :        THEN
               CONTINUE

            ELSE IF ( PARVPATH(J,NAMECODE) .EQ. SUBPAR__CURRENT ) THEN
               IF ( ( PARSTATE(NAMECODE) .EQ. SUBPAR__GROUND) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACCEPT) ) THEN
                  CALL SUBPAR_CURNAME ( NAMECODE, STRUCTNAME, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     FINISHED = .TRUE.
                  ELSE
                     CALL EMS_ANNUL( STATUS )
                  ENDIF

               ENDIF

            ELSE IF ( PARVPATH(J,NAMECODE) .EQ. SUBPAR__DEFAULT ) THEN

               IF ( ( PARSTATE(NAMECODE) .EQ. SUBPAR__GROUND ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESET ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACCEPT ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESACC ) ) THEN

                  IF ( ( PARDEF(3,NAMECODE) .GE. 20 )
     :            .OR. ( PARDEF(3,NAMECODE) .EQ. SUBPAR__CHAR ) ) THEN
                     STRUCTNAME = CHARLIST(PARDEF(1,NAMECODE))
                     STRUCTURE = .FALSE.
                     CALL SUBPAR_CHECKNAME ( STRUCTNAME, STRUCTURE,
     :                 STATUS )
                     IF ( STRUCTURE .AND. ( STATUS .EQ. SAI__OK ) ) THEN
                        FINISHED = .TRUE.
                     ELSE
                        CALL EMS_ANNUL( STATUS )
                     ENDIF
                  ELSE IF ( PARDEF(3,NAMECODE) .EQ. SUBPAR__NULLTYPE )
     :              THEN
                     STATUS = PAR__NULL
                     CALL EMS_SETC ( 'NAME', PARKEY(NAMECODE) )
                     CALL EMS_REP ( 'SUP_GETNAME4',
     :               'SUBPAR: Null (!) default value used for '//
     :               'parameter ^NAME', STATUS )
                     FINISHED = .TRUE.
                  ENDIF
               ENDIF

            ELSE IF ( PARVPATH(J,NAMECODE) .EQ. SUBPAR__DYNAMIC ) THEN

               IF ( ( ( PARSTATE(NAMECODE) .EQ. SUBPAR__GROUND ) .OR.
     :                ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESET ) .OR.
     :                ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACCEPT ) .OR.
     :                ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESACC ) )
     :          .AND. ( PARDYN(1,NAMECODE) .GT. 0 ) ) THEN
                  IF ( ( PARDYN(3,NAMECODE) .GE. 20 )
     :            .OR. (  PARDYN(3,NAMECODE) .EQ. SUBPAR__CHAR ) ) THEN
                     STRUCTNAME = CHARLIST(PARDYN(1,NAMECODE))
                     STRUCTURE = .FALSE.
                     CALL SUBPAR_CHECKNAME ( STRUCTNAME, STRUCTURE,
     :                 STATUS )
                     IF ( STRUCTURE .AND. ( STATUS .EQ. SAI__OK ) ) THEN
                        FINISHED = .TRUE.
                     ELSE
                        CALL EMS_ANNUL( STATUS )
                     ENDIF
                  ENDIF
               ENDIF

            ELSE IF ( PARVPATH(J,NAMECODE) .EQ. SUBPAR__GLOBAL ) THEN

               IF ( ( PARSTATE(NAMECODE) .EQ. SUBPAR__GROUND ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESET ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACCEPT ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESACC ) ) THEN
                  IF ( ( PARASSOC(2,NAMECODE) .EQ. SUBPAR__READ ) .OR.
     :              ( PARASSOC(2,NAMECODE) .EQ. SUBPAR__UPDATE ) ) THEN
                     CALL SUBPAR_NAMEASS ( NAMECODE, STRUCTNAME,
     :                 STATUS )
                     STRUCTURE = .FALSE.
                     CALL SUBPAR_CHECKNAME ( STRUCTNAME, STRUCTURE,
     :                 STATUS )
                     IF ( STRUCTURE .AND. ( STATUS .EQ. SAI__OK ) ) THEN
                        FINISHED = .TRUE.
                     ELSE
                        CALL EMS_ANNUL( STATUS )
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF

         ENDDO

         IF ( STATUS .EQ. SAI__OK ) THEN
*        If the parameter is not active or the preset value was a string,
*        set it to a name type.
            IF ( ( PARSTATE(NAMECODE) .NE. SUBPAR__ACTIVE )
     :       .OR. ( PARTYPE(NAMECODE) .LT. 20 ) ) THEN
               CALL SUBPAR_PUTNAME ( NAMECODE, STRUCTNAME, STATUS )
            ENDIF
*        save the current name (will save again if no intervening CANCL)
            CALL SUBPAR_CURSAV ( NAMECODE, STRUCTNAME, STATUS )
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            PARSTATE(NAMECODE) = SUBPAR__NULL
         ENDIF

      ENDIF
*
*   Release error context
*
      CALL EMS_RLSE

      END
