      SUBROUTINE SUBPAR_FINDKEY ( KEYWORD, SPECIAL, LOGICAL, NAMECODE,
     :                            STATUS )
*+
*  Name:
*     SUBPAR_FINDKEY

*  Purpose:
*     find the parameter corresponding to a keyword.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_FINDKEY ( KEYWORD, SPECIAL, LOGICAL, NAMECODE, STATUS )

*  Description:
*     Return the code-number of the parameter corresponding to the given
*     keyword. Upper case is assumed and, if environment variable ADAM_ABBRV
*     is set, abbreviations are allowed.
*     If argument SPECIAL is true, the given keyword is also checked against
*     the special keywords ACCEPT, PROMPT and RESET, subject to a minimum
*     of 2 characters.
*     If argument LOGICAL is  true only parameters of type LOGICAL are checked.
*     If a special keyword is matched, namecode will be returned negative:
*     -1 if ACCEPT  (or \)
*     -2 if PROMPT
*     -3 if RESET

*  Arguments:
*     KEYWORD=CHARACTER*(*) (given)
*        the keyword for the parameter
*     SPECIAL=LOGICAL (given)
*        if special keywords are to be included
*     LOGICAL=LOGICAL (given)
*        if we are only interested in LOGICAL parameters
*     NAMECODE=INTEGER (returned)
*        code number of the parameter
*     STATUS=INTEGER

*  Algorithm:
*     The given keyword is assumed to be in upper case and the PARKEY
*     array in SUBPAR common is searched for a match.
*     If LOGICAL is true, only keywords of LOGICAL parameters are checked
*     If SPECIAL is true the special keywords ACCEPT, PROMPT and RESET are
*     also included in the search (these are checked first).
*     Note that the keyword for a parameter will have been
*     defaulted to the parameter name if a keyword has not been
*     explicitly defined.
*     If an exact match is not found, search again to see if the given keyword
*     is an abbreviation of a keyword. As we already know that the strings
*     are in upper case, CHR_ABBRV is not used. A minimum abbreviation of two
*     characters is required for special keywords.
*     If more thatn one match is found, the user is informed and asked to
*     be more explicit. The given reply is converted to upper case and put
*     through the complete check again. If an un-ambiguous keyword is not
*     obtained after five attempts, SUBPAR__NOPAR is returned.

*  Copyright:
*     Copyright (C) 1984, 1987, 1991, 1992, 1993, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 1996, 2001 Central Laboratory of the Research Councils.
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
*     {enter_new_authors_here}

*  History:
*     09-NOV-1984 (BDK):
*        Original
*     28-JAN-1987 (JAB):
*        Make it work for monoliths
*     16-JUL-1991 (AJC):
*        Use CHR not STR$ for portability
*     22-JAN-1992 (AJC):
*        set SUBPAR not PARSE error
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      6-SEP-1994 (AJC):
*        Allow abbreviations.
*        Add arguments SPECIAL and LOGICAL
*     13-SEP-1995 (AJC):
*        Check against MOD(PARTYPE,10) to allow for
*         non-deactivated parameter system.
*      6-MAR-1996 (AJC):
*        Abort on !! as well as ! when getting unambiguous
*        keyword.
*     11-JUL-2001 (AJC):
*        Prevent prompting if ADAM_NOPROMPT set
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
      INCLUDE 'SUBPAR_ERR'


*  Arguments Given:
      CHARACTER*(*) KEYWORD           ! keyword to be found
      LOGICAL SPECIAL                 ! include special keywords in check
      LOGICAL LOGICAL                 ! only check LOGICAL keywords

*  Arguments Returned:
      INTEGER NAMECODE                ! codenumber for parameter


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  External routines:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN                      ! Used length of string

*  Local Variables:
      INTEGER MATCHES                       ! Number of matches
      INTEGER NCODE                         ! NAMECODE of first match
      INTEGER PROMPT                        ! Number of prompts
      INTEGER L1                            ! Length of KEYWORD
      INTEGER I                             ! Special Keyword pointer
      CHARACTER*(SUBPAR__NAMELEN) SKEYS(3)  ! Special Keywords
      CHARACTER*(SUBPAR__NAMELEN) INNAME    ! Copy of KEYWORD
      CHARACTER*80 ABBRV                    ! ADAM_ABBRV string
      LOGICAL FOUND                         ! search loop controller

*  Local Data:
      DATA SKEYS/'ACCEPT','PROMPT','RESET'/
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Set error context
      CALL EMS_MARK

*   Initialize for the search
      FOUND = .FALSE.
      INNAME = KEYWORD
      PROMPT = 0

*   For the given keyword and any supplied as  a result of
*   prompting from this routine, keep trying until error or un-ambiguous
*   keyword is found
      DO WHILE ( (.NOT. FOUND ) .AND. ( STATUS .EQ. SAI__OK ) )

*     Initialise the match count
         MATCHES = 0

*      Find the number of characters given, ignoring trailing blanks, but
*      using at least 1 character.
         L1 = MAX( 1, CHR_LEN( INNAME ) )

*      First look for an exact match
*      Search the list of keywords for matches
*
*      First the special keywords if required
         IF ( SPECIAL ) THEN
            I = 0
*         Treat backslash separately
            IF ( ( L1 .EQ. 1 ) .AND. (INNAME(1:1) .EQ. CHAR(92) ) )
     :      THEN
               FOUND = .TRUE.
               MATCHES = 1
               NCODE = -1
            END IF

*         Check for ACCEPT, PROMPT and RESET
            DO WHILE (  ( .NOT. FOUND ) .AND.
     :                  ( I .LT. 3 ) )
               I = I + 1
*           If it matches, end loop and set NAMECODE negative value
*           to indicate which special keyword
               IF ( INNAME .EQ. SKEYS( I ) ) THEN
                  FOUND = .TRUE.
                  MATCHES = 1
                  NCODE = - I
               END IF
            END DO
         END IF

*      Now the parameter keywords
         NAMECODE = PROGADD(1,PROGNUM) - 1
         DO WHILE (  ( .NOT. FOUND ) .AND.
     :               ( NAMECODE .LT. PROGADD(2,PROGNUM) ) )

            NAMECODE = NAMECODE + 1
            IF ( .NOT. ( LOGICAL
     :      .AND. ( MOD(PARTYPE( NAMECODE ),10)
     :             .NE. SUBPAR__LOGICAL ) ) )
     :      THEN
               IF ( INNAME .EQ. PARKEY( NAMECODE ) ) FOUND = .TRUE.
            END IF

         END DO

*      If an exact match wasn't found, try for an abbreviation.
         IF ( .NOT. FOUND ) THEN
*         First check that abbreviations are permitted
            CALL PSX_GETENV( 'ADAM_ABBRV', ABBRV, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
*           Abbreviations are not permitted
               CALL EMS_ANNUL( STATUS )

            ELSE
*           Abbreviations are permitted

*           First check the special keywords if required
               IF ( ( SPECIAL ) .AND. ( L1 .GE. 2 ) ) THEN
                  I = 0
                  DO WHILE ( ( I .LT. 3 ) .AND. ( MATCHES .EQ. 0 ) )
                     I = I + 1
                     IF ( INNAME( : L1 ) .EQ. SKEYS( I )( : L1 ) )
     :               THEN
                        MATCHES = MATCHES + 1
*                    If a match is found here, it must be the first match
*                    so set NCODE indicating which special keyword.
                        NCODE = - I
                     END IF
                  END DO
               END IF

*           Now check the parameter keywords
               NAMECODE = PROGADD(1,PROGNUM) - 1
               DO WHILE ( NAMECODE .LT. PROGADD(2,PROGNUM) )

                  NAMECODE = NAMECODE + 1

                  IF ( .NOT. ( LOGICAL .AND.
     :            ( MOD(PARTYPE( NAMECODE ),10)
     :              .NE. SUBPAR__LOGICAL ) ) )
     :            THEN
                     IF
     :               ( INNAME( : L1 ) .EQ. PARKEY( NAMECODE )( : L1 ) )
     :               THEN
                        MATCHES = MATCHES + 1

*                    If more than one match prepare the error messages
                        IF ( MATCHES .GT. 1 ) THEN
                           STATUS = SUBPAR__NOPAR
                           IF ( MATCHES .EQ. 2 ) THEN
                              CALL EMS_SETC( 'NAME', INNAME )
                              CALL EMS_REP( 'SUP_FINDKEY1',
     :                        'Ambiguous keyword ^NAME used on the ' //
     :                        'command line', STATUS )
                              IF ( NCODE .LT. 0 ) THEN
                                 CALL EMS_SETC( 'NAME', SKEYS(-NCODE) )
                              ELSE
                                 CALL EMS_SETC( 'NAME', PARKEY(NCODE) )
                              END IF
                              CALL EMS_REP( 'SUP_FINDKEY2',
     :                        'Matches with ^NAME', STATUS )
                           END IF
                           CALL EMS_SETC( 'NAME', PARKEY( NAMECODE ) )
                           CALL EMS_REP( 'SUP_FINDKEY3',
     :                     '         and ^NAME', STATUS )

                        ELSE
*                       Save the NAMECODE of the first (or only) match
                           NCODE = NAMECODE

                        END IF

                     END IF

                  END IF

               ENDDO

            END IF

            IF ( MATCHES .EQ. 1 ) THEN
*        This is the one - terminate the loop
               FOUND = .TRUE.

            ELSE IF ( MATCHES .EQ. 0 ) THEN
*        There were no matches
               STATUS = SUBPAR__NOPAR
               CALL EMS_SETC ( 'NAME', INNAME )
               CALL EMS_REP ( 'SUP_FINDKEY4', 'SUBPAR: ' //
     :         'Keyword ^NAME is not defined in the interface file',
     :          STATUS )
               NAMECODE = 0
*        If this keyword was obtained by prompting, force another prompt to
*        give the user another chance.
               IF ( PROMPT .GT. 0 ) MATCHES = 2

            END IF

            IF ( MATCHES .GT. 1 ) THEN
               PROMPT = PROMPT + 1
               IF ( PROMPT .GT. 5 ) THEN
                  STATUS = SUBPAR__NOPAR
                  CALL EMS_REP( 'SUP_FINDKEY5',
     :            'SUBPAR: Five prompts failed to get a valid keyword',
     :             STATUS )

               ELSE

*              Display the list of options or other error message.
                  CALL SUBPAR_EFLSH( STATUS )

                  CALL PSX_GETENV( 'ADAM_NOPROMPT', ABBRV, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN
*              If ADAM_NOPROMPT is set abort
                     STATUS = SUBPAR__NOPAR
                     CALL EMS_REP ( 'SUP_FINDKEY4', 'SUBPAR: ' //
     :               'Prompting for unambiguous value prohibited',
     :                STATUS )
                     NAMECODE = 0


                  ELSE
*              otherwise cancel the error message from PSX_GETENV
                     CALL EMS_ANNUL( STATUS )
*              and prompt for an un-ambiguous choice
                     CALL SUBPAR_KEYREQ( INNAME, STATUS )
*              Check for abortion
*              In this case the keyword will be treated as it is
*              1. For the KAMBIG case it will be assumed not to be a keyword
*              2. For the KEYWORD case it will cause an error.
                     IF ( ( INNAME .EQ. '!' ) .OR.
     :                    ( INNAME .EQ. '!!' ) )THEN
                        STATUS = SUBPAR__NOPAR
                        CALL EMS_SETC( 'NAME', KEYWORD )
                        CALL EMS_REP( 'SUP_FINDKEY6',
     :                  'SUBPAR: Aborted attempt to re-specify ' //
     :                  'ambiguous keyword ^NAME', STATUS )
                     ELSE
*                 Force the new keyword to upper case for checking
                        CALL CHR_UCASE( INNAME )
                     END IF
                  END IF

               END IF
*           Now check out the new keyword

            END IF

         END IF

      END DO

*  Now set NAMECODE appropriately
      IF ( MATCHES .EQ. 1 ) NAMECODE = NCODE

*  Release error context
      CALL EMS_RLSE

      END
