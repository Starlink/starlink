      SUBROUTINE SUBPAR_INPUT ( NAMECODE, VALUE, STATUS)
*+
*  Name:
*     SUBPAR_INPUT

*  Purpose:
*     Get text string input with prompt.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_INPUT ( NAMECODE, VALUE, STATUS)

*  Description:
*     A text-string value is returned for the indicated parameter, after
*     generating a prompt to a user.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to program parameter
*     VALUE=CHARACTER*(*) (returned)
*        value obtained for parameter
*     STATUS=INTEGER

*  Algorithm:
*     The prompt information for the parameter is obtained and passed to
*     the get-input-with-prompt routine.

*  Copyright:
*     Copyright (C) 1984, 1987, 1990, 1991, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     24-SEP-1984 (BDK):
*        Original
*     06-MAY-1987 (BDK):
*        detect a default of NULL
*     08-MAY-1987 (BDK):
*        follow search-path for prompt value
*     28-MAY-1987 (BDK):
*        handle parameters in RESET states
*     28-MAY-1987 (BDK):
*        handle parameters in ACCEPT states
*     08-JUN-1987 (BDK):
*        prompt using keyword
*     23-MAY-1987 (AJC):
*        enable global in ppath
*     14-MAY-1990 (AJC):
*        add multi-line help facility
*     09-JUL-1990 (AJC):
*        use 'type' as dynamic default exists flag
*     05-AUG-1991 (AJC):
*        annul failures to get suggested (prompt) value
*     25-SEP-1991 (AJC):
*        correctly mark and rlse error context
*     20-NOV-1991 (AJC):
*        prompt if ACCEPT but no suggested value
*     14-JAN-1992 (AJC):
*        error report on ! default accepted
*     10-JUL-1992 (AJC):
*        add \ feature to force accept on current and other non-active
*        parameters
*        increase DEFAULT size to accomodate max CHAR default
*     19-AUG-1992 (AJC):
*        for dynamics check PARDYN(1,-) also
*     26-FEB-1993 (AJC):
*        Use CHAR(92) not backslash char for portability
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     16-MAR-1993 (AJC):
*        Allow new state RESACCPR
*        Change name SUBPAR_ACCPR to SUBPAR_ACCPT
*      9-AUG-1993 (AJC):
*        INCLUDE SUBPAR_PARERR not PAR_ERR
*     21-DEC-1995 (AJC):
*        Increase max suggested value length to SUBPAR__STRLEN+2
*        (was 132+2).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'
      INCLUDE 'SUBPAR_PARERR'


*  Arguments Given:
      INTEGER NAMECODE               ! parameter number


*  Arguments Returned:
      CHARACTER*(*) VALUE            ! parameter value


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Variables:
      CHARACTER*(SUBPAR__STRLEN+2) DEFAULT     ! value shown in prompt string

      INTEGER J                      ! counter for search-path

      LOGICAL FINISHED               ! controller for search-path


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set error context
      CALL EMS_MARK
*
*   Try to find a default value to be included in the prompt following
*   the PPATH
*
      FINISHED = .FALSE.
      J = 0

      DO WHILE ( ( .NOT. FINISHED ) .AND. ( J .LT. 5 ) )

         J = J + 1

         IF ( PARPPATH(J,NAMECODE) .EQ. SUBPAR__CURRENT ) THEN
*         Provided it't not a RESET state - try to get current value
            IF ( ( PARSTATE(NAMECODE) .NE. SUBPAR__RESET ) .AND.
     :        ( PARSTATE(NAMECODE) .NE. SUBPAR__RESACC ) .AND.
     :        ( PARSTATE(NAMECODE) .NE. SUBPAR__RESACCPR ) .AND.
     :        ( PARSTATE(NAMECODE) .NE. SUBPAR__RESPROM ) ) THEN
               CALL SUBPAR_CURVAL ( NAMECODE, DEFAULT, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  FINISHED = .TRUE.
               ELSE
                  CALL EMS_ANNUL ( STATUS )
               ENDIF
            ENDIF

         ELSE IF ( PARPPATH(J,NAMECODE) .EQ. SUBPAR__DEFAULT ) THEN
            IF ( PARDEF(1,NAMECODE) .GT. 0 ) THEN
               CALL SUBPAR_CONVALS ( PARDEF(1,NAMECODE),
     :           PARDEF(2,NAMECODE), PARDEF(3,NAMECODE), DEFAULT,
     :             STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  FINISHED = .TRUE.
               ELSE
                  CALL EMS_ANNUL ( STATUS )
               ENDIF
            ELSE IF ( PARDEF(3,NAMECODE) .EQ. SUBPAR__NULLTYPE ) THEN
               DEFAULT = '!'
               FINISHED = .TRUE.
            ENDIF

         ELSEIF ( PARPPATH(J,NAMECODE) .EQ. SUBPAR__GLOBAL ) THEN
*         Try to get global value
            IF ( ( PARASSOC(2, NAMECODE) .EQ. SUBPAR__READ ) .OR.
     :         ( PARASSOC(2, NAMECODE) .EQ. SUBPAR__UPDATE ) ) THEN
               CALL SUBPAR_VALASS ( NAMECODE, DEFAULT, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  FINISHED = .TRUE.
               ELSE
                  CALL EMS_ANNUL ( STATUS )
               ENDIF
            ENDIF

         ELSE IF ( PARPPATH(J,NAMECODE) .EQ. SUBPAR__DYNAMIC ) THEN
            IF ( ( PARDYN(1,NAMECODE) .GT. 0 )
     :      .AND.( PARDYN(3,NAMECODE) .GT. 0 ) ) THEN
               CALL SUBPAR_CONVALS ( PARDYN(1,NAMECODE),
     :           PARDYN(2,NAMECODE), PARDYN(3,NAMECODE), DEFAULT,
     :           STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  FINISHED = .TRUE.
               ELSE
                  CALL EMS_ANNUL ( STATUS )
               ENDIF
            ENDIF

         ENDIF

      ENDDO

      IF ( .NOT. FINISHED ) THEN
*
*      Search-path failed (eg there wasn't one), use standard search.
*
         IF ( (PARDYN(1,NAMECODE) .GT. 0 )
     :   .AND.( PARDYN(3,NAMECODE) .GT. 0 ) ) THEN
            CALL SUBPAR_CONVALS ( PARDYN(1,NAMECODE),
     :        PARDYN(2,NAMECODE), PARDYN(3,NAMECODE), DEFAULT, STATUS )
         ELSE IF ( PARDEF(1,NAMECODE) .GT. 0 ) THEN
            CALL SUBPAR_CONVALS ( PARDEF(1,NAMECODE),
     :        PARDEF(2,NAMECODE), PARDEF(3,NAMECODE), DEFAULT, STATUS )
         ELSE IF ( PARDEF(3,NAMECODE) .EQ. SUBPAR__NULLTYPE ) THEN
            DEFAULT = '!'
         ELSE
            DEFAULT = ' '
         ENDIF

      ENDIF
*
*   Handle ACCEPT states
*
      IF ( ( ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACCEPT ) .OR.
     :  ( PARSTATE(NAMECODE) .EQ. SUBPAR__ACCPR ) .OR.
     :  ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESACC ) .OR.
     :  ( PARSTATE(NAMECODE) .EQ. SUBPAR__RESACCPR ) ) .AND.
     :  ( DEFAULT .NE. ' ' ) ) THEN
*
*      Accept default automatically
*
         IF ( DEFAULT .EQ. '!' ) THEN
            STATUS = PAR__NULL
            CALL EMS_SETC( 'NAME', PARKEY(NAMECODE) )
            CALL EMS_REP( 'SUP_INPUT1',
     :      'SUBPAR: Null (!) parameter default value accepted for '//
     :      'parameter ^NAME', STATUS )
         ELSE
            VALUE = DEFAULT
         ENDIF
      ELSE
*
*   Send all relevant information to the get-with-prompt routine
*
         CALL SUBPAR_REQUEST ( PARKEY(NAMECODE), PARPROM(NAMECODE),
     :     DEFAULT, PARHELP(NAMECODE), PARHKEY(NAMECODE),
     :     PARERRMESS, VALUE, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN
*        Check for the special '\' (ACCEPT) response.
*        If found, set the state of other parameters appropriately
*        and set VALUE to the suggested value if there is one.
*        If the default is ! or blank, set STATUS NULL.
            IF ( VALUE .EQ. CHAR(92) ) THEN
               CALL SUBPAR_ACCPT( STATUS )
               IF ( (DEFAULT .EQ. '!')
     :         .OR. (DEFAULT .EQ. ' ') ) THEN
                  STATUS = PAR__NULL
               ELSE
                  VALUE = DEFAULT
               ENDIF
            ENDIF
         ENDIF
      ENDIF
*
*   Cancel any pre-set error message
*
      PARERRMESS = ' '

*   Release error context
      CALL EMS_RLSE

      END
