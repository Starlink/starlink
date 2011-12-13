      SUBROUTINE SUBPAR_FINDHDS ( NAMECODE, ACCESS, LOC, STATUS )
*+
*  Name:
*     SUBPAR_FINDHDS

*  Purpose:
*     Get a valid data structure for a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_FINDHDS ( NAMECODE, ACCESS, LOC, STATUS )

*  Description:
*     Follows the VPATH for the indicated parameter to obtain an HDS
*     locator to the data for the parameter.
*     Failure to get a locator causes the next step in the VPATH to
*     be tried.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to the parameter
*     ACCESS=CHARACTER*(*) (given)
*        access required - READ, WRITE or UPDATE
*     LOC=CHARACTER*(DAT__SZLOC) (returned)
*        locator to data structure component
*     STATUS=INTEGER

*  Algorithm:
*     First trap the case wher parameter state is NULL.
*     If it isn't check for other states (i.e MIN, MAX, ACTIVE) which
*     indicate a value is already specified - attempt to get the indicated
*     value.
*     Otherwise, or if that fails, check in turn the data sources suggested
*     by the VPATH until one of them gives a valid result.
*     If the vpath is overridden, by CANCEL, FPROMPT etc., prompt the
*     user. Prompt up to five times.
*     If a valid locator is not obtained, return status PAR__NULL.

*  Implementation Deficiencies:
*     This allows an extension to the SSE 0.75 definition of VPATH.
*     This is INTERNAL, which causes access to the data
*     structure attached to the application.
*     CURRENT also takes values from this data structure. However,
*     CURRENT should only take values created the last time the
*     application was SUCCESSFULLY run. The way it is implemented here,
*     it will take whatever value exists.
*     * The 12/6/90 CJM fix is to cover a case which only happens when
*     * a CD-task doesn't switch off the NEEDS check. The problem should
*     * be corrected in a different way - then this fix can be removed.
*     The setting of PARERRMESS should be improved

*  Copyright:
*     Copyright (C) 1984, 1985, 1987, 1989, 1990, 1991, 1992, 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     CJM: C J Mayer (JACH)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-SEP-1984 (BDK):
*        Original version.
*     17-APR-1985 (BDK):
*        Add CURRENT.
*     18-APR-1985 (BDK):
*        Put error message in COMMON if prompting failed.
*     06-MAY-1987 (BDK):
*        Put error message if parameter ACTIVE but with bad
*        value - e.g. from faulty command-line.
*     08-MAY-1987 (BDK):
*        Allow for NULL default.
*     28-MAY-1987 (BDK):
*        Add VPATH overrides.
*     10-JUN-1987 (BDK):
*        Trap bad status on overridden VPATH.
*     10-JUN-1987 (BDK):
*        Allow RESET parameters to follow VPATH.
*     30-JUL-1987 (BDK):
*        Force prompting if command-line error.
*     18-AUG-1987 (BDK):
*        Make 'current' work for HDS names.
*     26-APR-1989 (AJC):
*        Don't follow vpath in CANCELLED state.
*        Let RESET ignore 'current' on vpath also.
*     12-JUN-1990 (CJM):
*        Allow case 'state ACTIVE' if vpath is current or default
*        (See Deficiencies).
*     11-SEP-1990 (AJC):
*        Handle vpath overridden inside the loop.
*        Allow 5 re-prompts on bad values.
*     18-JUL-1991 (AJC):
*        Use EMS not LIB$GETMSG.
*     30-JUL-1991 (AJC):
*        Re-organize error reporting.
*        Add message on prompt count exceeded.
*     24-SEP-1991 (AJC):
*        Prefix messages with 'SUBPAR:'
*        and ! etc. on output.
*     14-JAN-1992 (AJC):
*        Add message when vpath exhausted.
*     26-AUG-1992 (PCTR):
*        Replaced EMS_ELOAD/SUBPAR_WRITE loop with a call to
*        SUBPAR_EFLSH.
*      9-SEP-1992 (AJC):
*        Don't GETLOC for internals - just ignore the first vpath spec
*        otherwise the 'current' value is always taken even if it has not
*        been set on this invocation.
*     16-NOV-1992 (AJC):
*        Make ACCPR not override VPATH
*     17-NOV-1992 (AJC):
*        Add MIN/MAX
*        Modify NULL already set message
*     10-MAR-1993 (AJC):
*        Add DAT_PAR for SUBPAR_CMN
*     16-MAR-1993 (AJC):
*        Allow for new states ACCEPT, RESACCPR
*        Remove ACTIVE state from possibilities in vpath search
*      9-AUG-1993 (AJC):
*        INCLUDE SUBPAR_PARERR not PAR_ERR
*      4-AUG-1994 (AJC):
*        Also terminate on PAR__NOUSR from prompt.
*     16-DEC-1998 (AJC):
*        Assume string if no MIN/MAX for character parameters
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
      INCLUDE 'LEX_PAR'

*  Arguments Given:
      INTEGER NAMECODE                ! pointer to parameter

      CHARACTER*(*) ACCESS            ! access type


*  Arguments Returned:
      CHARACTER*(DAT__SZLOC) LOC      ! locator to HDS structure


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      CHARACTER*132 STRUCTNAME        ! structure name
      INTEGER J                       ! stage on VPATH
      LOGICAL FINISHED                ! loop controller

*.

*  Check the inherited status.
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
         CALL EMS_SETC ( 'NAME', PARKEY(NAMECODE) )
         CALL EMS_REP ( 'SUP_FINDHDS1',
     :   'SUBPAR: Parameter ^NAME is in NULL state', STATUS )


      ELSE
*
*   Start the search with the current parameter value
*
         FINISHED = .FALSE.
         IF ( ( PARTYPE(NAMECODE) .GE. 20 ) .AND.
     :     ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACTIVE ) ) THEN
*
*      A name has already been stored with the parameter
*
            STRUCTNAME = PARVALS(NAMECODE)
            CALL SUBPAR_GETHDS ( NAMECODE, STRUCTNAME, ACCESS, LOC,
     :        STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               FINISHED = .TRUE.
            ELSE
*
*            Report error message to be displayed before any re-prompt
*            This condition probably has arisen because of a typing
*            error on the command-line, so force prompting rather than
*            follow the VPATH.
*
               CALL EMS_SETC ( 'NAME', PARKEY(NAMECODE) )
               CALL EMS_REP ( 'SUP_FINDHDS2',
     :         'SUBPAR: Bad preset value for parameter ^NAME', STATUS )
               PARSTATE(NAMECODE) = SUBPAR__FPROMPT
            ENDIF


*      Else check for State SUBPAR__MIN
         ELSE IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__MIN ) THEN

            CALL SUBPAR_MNMX( NAMECODE, 'MIN', STATUS )
            CALL DAT_CLONE( PARLOC(2,NAMECODE), LOC, STATUS )
            IF ( STATUS .EQ. SAI__OK ) FINISHED = .TRUE.

*      Else check for State SUBPAR__MAX
         ELSE IF ( PARSTATE(NAMECODE) .EQ. SUBPAR__MAX ) THEN

            CALL SUBPAR_MNMX( NAMECODE, 'MAX', STATUS )
            CALL DAT_CLONE( PARLOC(2,NAMECODE), LOC, STATUS )
            IF ( STATUS .EQ. SAI__OK ) FINISHED = .TRUE.

         ENDIF

*      Check for MIN/MAX on character strings with no MIN/MAX set
*      Assume it was meant to be a string in that case.
         IF( ( STATUS .EQ. SUBPAR__NOMNMX ) .AND.
     :       ( PARTYPE(NAMECODE) .EQ. SUBPAR__CHAR ) ) THEN
            CALL EMS_ANNUL( STATUS )
            CALL SUBPAR_STORE0( NAMECODE, LEX__STRING,
     :        PARVALS(NAMECODE), 3, LOC, STATUS )
            IF ( STATUS .EQ. SAI__OK ) FINISHED = .TRUE.
         ENDIF
*
*      Try to get the name from elsewhere, following the VPATH
*      Possible steps on the path are
*      Path exhausted => PAR__NULL ( should never happen. error?)
*      NOPROMPT       => PAR__NULL
*      PROMPT         => prompt
*      NOPATH         => prompt
*      DEFAULT        => static default
*      DYNAMIC        => dynamic default
*      GLOBAL         => get from ASSOCIATION structure
*      INTERNAL       => program 'private' storage
*      CURRENT        => program 'private' storage

         J = 0

         DO WHILE ( .NOT. FINISHED )

*        Can get here with state GROUND, RESET (or EOL?) or CANCEL
            J = J + 1

*        Flush any pending error messages.
            CALL SUBPAR_EFLSH( STATUS )
            STATUS = SAI__OK

*        Handle cases where VPATH has been overidden or VPATH specifies
*        prompt or has expired.
*        Allow up to 5 attempts.
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

*           Use count 6 to 10 for prompt repeats
               IF ( J .LE. 5 )  J = 6

               CALL SUBPAR_HDSIN ( NAMECODE, ACCESS, LOC, STATUS )

*           If value obtained OK (or completion status), terminate loop.
*           Otherwise STATE will have been set to PAR__CANCEL, causing
*           reprompt until counter expires.
               IF ( ( STATUS .EQ. SAI__OK ) .OR.
     :           ( STATUS .EQ. PAR__NULL ) .OR.
     :           ( STATUS .EQ. PAR__ABORT ) .OR.
     :           ( STATUS .EQ. PAR__NOUSR ) ) THEN
                  FINISHED = .TRUE.

               ENDIF

            ELSEIF ( ( J .GT. 5 ) .OR. ( PARVPATH(J,NAMECODE) .EQ.
     :        SUBPAR__NOPROMPT) ) THEN
*            Give up trying to get a value. Set PAR__NULL and reset
*            error message
               FINISHED = .TRUE.
               PARERRMESS = ' '
               STATUS = PAR__NULL
*            If due to maximum re-prompt failure, report it
               IF ( J .GT. 10 ) THEN
                  CALL EMS_SETC ( 'NAME', PARKEY(NAMECODE) )
                  CALL EMS_REP ( 'SUP_FINDHDS3',
     :            'SUBPAR: 5 prompts failed to get a good value for '//
     :            'parameter ^NAME - NULL assumed', STATUS )

*            Else if NOPROMPT on vpath
               ELSE
                  CALL EMS_SETC ( 'NAME', PARKEY(NAMECODE) )
                  CALL EMS_REP ( 'SUP_FINDHDS4',
     :            'SUBPAR: NOPROMPT on vpath for parameter ^NAME - '//
     :            'NULL assumed', STATUS )
               ENDIF

            ELSE IF ( PARVPATH(J,NAMECODE) .EQ. SUBPAR__CURRENT ) THEN
               IF ( ( PARSTATE(NAMECODE) .EQ. SUBPAR__GROUND) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACCEPT) ) THEN
                  CALL SUBPAR_CURLOC ( NAMECODE, ACCESS, LOC, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     FINISHED = .TRUE.
                  ELSE
                     CALL EMS_ANNUL ( STATUS )
                  ENDIF
               ENDIF

            ELSE IF ( PARVPATH(J,NAMECODE) .EQ. SUBPAR__INTERNAL ) THEN
*           We only come here if _GET1/V/N are called and there is no
*           preset value.
*           PARSECON sets up a full vpath for 'internal' parameters so we
*           can just continue the search for a value
               CONTINUE

            ELSE IF ( PARVPATH(J,NAMECODE) .EQ. SUBPAR__DEFAULT ) THEN

               IF ( ( PARSTATE(NAMECODE) .EQ. SUBPAR__GROUND ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESET ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACCEPT ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESACC ) ) THEN
                  CALL SUBPAR_HDSDEF ( NAMECODE, ACCESS, LOC, STATUS )
                  IF ( ( STATUS .EQ. SAI__OK ) .OR.
     :              ( STATUS .EQ. PAR__NULL ) ) THEN
                     FINISHED = .TRUE.
                  ELSE
                     CALL EMS_ANNUL ( STATUS )
                  ENDIF
               ENDIF

            ELSE IF ( PARVPATH(J,NAMECODE) .EQ. SUBPAR__DYNAMIC ) THEN

               IF ( ( PARSTATE(NAMECODE) .EQ. SUBPAR__GROUND ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESET ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACCEPT ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESACC ) ) THEN
                  CALL SUBPAR_HDSDYN ( NAMECODE, ACCESS, LOC, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     FINISHED = .TRUE.
                  ELSE
                     CALL EMS_ANNUL ( STATUS )
                  ENDIF
               ENDIF

            ELSE IF ( PARVPATH(J,NAMECODE) .EQ. SUBPAR__GLOBAL ) THEN

               IF ( ( PARSTATE(NAMECODE) .EQ. SUBPAR__GROUND ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESET ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACCEPT ) .OR.
     :              ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESACC ) ) THEN
                  IF ( ( PARASSOC(2,NAMECODE) .EQ. SUBPAR__READ ) .OR.
     :              ( PARASSOC(2,NAMECODE) .EQ. SUBPAR__UPDATE ) ) THEN
                     CALL SUBPAR_HDSASS ( NAMECODE, ACCESS, LOC,
     :                 STATUS )
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        FINISHED = .TRUE.
                     ELSE
                        CALL EMS_ANNUL ( STATUS )
                     ENDIF
                  ENDIF
               ENDIF

            ENDIF

         ENDDO

      ENDIF

*
*  Restore error context
*
      CALL EMS_RLSE

      END
