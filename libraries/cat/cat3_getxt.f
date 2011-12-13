      SUBROUTINE CAT3_GETXT (CI, FINISH, CLASS, TEXT, STATUS)
*+
*  Name:
*     CAT3_GETXT
*  Purpose:
*     Get the next line of textual information from a FITS table.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT3_GETXT (CI; FINISH, CLASS, TEXT; STATUS)
*  Description:
*     Get the next line of textual information from a FITS table.
*  Arguments:
*     CI  =  INTEGER (Given)
*         Catalogue identifier.
*     FINISH   =  LOGICAL (Returned)
*         Flag indicating whether a line of text was obtained, coded as
*         follows:
*         .FALSE. -  a line was obtained ok; input of of textual
*                    information continues.
*         .TRUE.  -  all the textual information has already been
*                    returned; input of textual information has
*                    terminated.
*     CLASS   =  CHARACTER*(*) (Returned)
*         Class of the textual information.  A set of values are
*         permitted for each type of catalogue back-end (or file
*         format), and these sets are different for different back-ends.
*         Note that this argument is returned rather than given; an
*         application cannot prescribe to CAT what class of textual
*         information is required.
*     TEXT   =  CHARACTER*(*) (Returned)
*         A single line of textual information.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Determine the common block array element for the catalogue.
*     Determine the FITS unit number for accessing the catalogue.
*     Determine the current state of access to textual information.
*     If the current state is 'before the primary header' then
*       Set the finished flag to unfinished.
*       Set the text to 'Primary header:-'.
*       Set the class to 'CAT'.
*       Move to the primary header.
*       Determine the number of keywords in the primary header.
*       Set the number of the current keyword to 0.
*       Update the state to 'in the primary header.
*     else if the current state is 'in the primary header' then
*       Increment to the next keyword.
*       If there are more keywords to read then
*         Attempt to get the text for this keyword.
*         If ok then
*           Set the finished flag to unfinished.
*           Set the text to the keyword text.
*           Set the class.
*         else
*           Report an error.
*           Set the finished flag.
*           Set the text to blank.
*           Set the class to blank.
*         end if
*       else (there are no more keywords)
*         Set the finished flag to unfinished.
*         Set the text to 'Table extension header:-'.
*         Set the class to 'CAT'.
*         Move to the extension header.
*         Determine the number of keywords in the header.
*         Set the current keyword to 0.
*         Update the state to 'in the extension header'.
*       end if
*     else if the current state is 'in the extension header' then
*       Increment to the next keyword.
*       If there are more keywords to read then
*         Attempt to get the text for this keyword.
*         If ok then
*           Set the finished flag to unfinished.
*           Set the text to the keyword text.
*           Set the class.
*         else
*           Report an error.
*           Set the finished flag.
*           Set the text to blank.
*           Set the class to blank.
*         end if
*       else (there are no more keywords)
*         Set the finished flag.
*         Set the text to blank.
*         Set the class to blank.
*       end if
*     else the current state is invalid.
*       Set the status.
*       Set the finished flag.
*       Set the text to blank.
*       Set the class to blank.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     22/9/94  (ACD): Original version.
*     20/3/96  (ACD): Fixed bugs; variable FITSTT was not being
*        initialised and in a couple of places the ADAM status variable
*        was being used instead of the FITSIO one.
*     25/11/96 (ACD): Moved the initialisation of FITSTT to the correct
*        location.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
      INCLUDE 'CAT1_PAR'          ! Internal CAT constants.
      INCLUDE 'CAT_ERR'           ! CAT error codes.
      INCLUDE 'CAT3_FIT_PAR'      ! FITS back-end constants.
*  Global Variables:
      INCLUDE 'CAT3_FIT_CMN'      ! FITS back-end common block.
*  Arguments Given:
      INTEGER
     :  CI
*  Arguments Returned:
      LOGICAL
     :  FINISH
      CHARACTER
     :  CLASS*(*),
     :  TEXT*(*)
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Constants:
      INTEGER FITOK          ! FITSIO success status.
      PARAMETER (FITOK = 0)
*  Local Variables:
      INTEGER
     :  CIELM,   ! Element for the catalogue in the common block arrays.
     :  FITUNT,  ! Unit number for accessing FITS table.
     :  HSTATE,  ! Current state accessing the text information.
     :  FITSTT,  ! FITSIO status.
     :  HDUTYP,  ! FITSIO coded for the type of the current header.
     :  NKEYWD,  ! Number of keywords in the current header.
     :  KEYADD,  ! Number of keywords that can be added to the header.
     :  ERRLEN   ! Length ofERRTXT (excl. trail. blanks).
      CHARACTER
     :  KCARD*80,  ! Text for current keyword record.
     :  ERRTXT*75  ! Error message text.
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Determine the common block array element for the catalogue.

         CALL CAT1_CIELM (CI, CIELM, STATUS)

*
*       Determine the FITS unit number for accessing the catalogue and
*       the current state of access to textual information from the
*       common block.

         FITUNT = FUNT__CAT3(CIELM)
         HSTATE = HSTAT__CAT3(CIELM)

         FITSTT = FITOK


*
*       Check for the various possible states of access to the current
*       header.

         IF (HSTATE .EQ. CAT3__HSTTB) THEN

*
*          The state is 'before the primary header'.  Set the text to
*          indicate that the primary header follows and set the class
*          to 'CAT' (indicating that this line of text has been
*          invented by CAT).

            FINISH = .FALSE.
            TEXT = 'Primary Header:-'
            CLASS = 'CAT'

*
*          Move to the primary header.

            CALL FTMAHD (FITUNT, 1, HDUTYP, FITSTT)
            IF (FITSTT .NE. FITOK) THEN
               STATUS = CAT__ERROR
               CALL CAT3_FITER ('CAT3_GETXT_AHD', 'Failed to access '/
     :           /'the FITS primary header.', FITSTT, STATUS)
            END IF

*
*          Determine the number of keywords in the primary header and
*          set the number of the current keyword to 0.  Also, update
*          the state to 'in primary header'.

            CALL FTGHSP (FITUNT, NKEYWD, KEYADD, FITSTT)
            IF (FITSTT .EQ. FITOK) THEN
               NUMKY__CAT3(CIELM) = NKEYWD
               CURKY__CAT3(CIELM) = 0
               HSTAT__CAT3(CIELM) = CAT3__HSTTP

            ELSE
               STATUS = CAT__ERROR
               CALL CAT3_FITER ('CAT3_GETXT_HSP', 'Failed to determine'/
     :           /' the number of keywords in the primary header.',
     :           FITSTT, STATUS)

            END IF

         ELSE IF (HSTATE .EQ. CAT3__HSTTP) THEN

*
*          The current state is 'in the primary header'.  Increment to
*          the next keyword and check if this keyword exists.

            CURKY__CAT3(CIELM) = CURKY__CAT3(CIELM) + 1

            IF (CURKY__CAT3(CIELM) .LE. NUMKY__CAT3(CIELM) ) THEN

*
*             Attempt to get the text for this keyword and proceed if
*             ok.

               CALL FTGREC (FITUNT, CURKY__CAT3(CIELM), KCARD, FITSTT)

               IF (FITSTT .EQ. FITOK) THEN

*
*                Set the finished flag to 'unfinished', accept the
*                contents of the header record as the text and determine
*                its class.

                  FINISH = .FALSE.
                  TEXT = KCARD
                  CALL CAT3_GTXCL (KCARD, CLASS, STATUS)

               ELSE

*
*                Set the status, report an error and set the return
*                variables.

                  STATUS = CAT__ERROR

                  CALL CAT3_FITER ('CAT3_GETXT_CRP', 'Error getting '/
     :              /'a record from the primary header.', FITSTT,
     :              STATUS)

                  FINISH = .TRUE.
                  TEXT = ' '
                  CLASS = ' '

               END IF

            ELSE

*
*             All the keywords in the primary header have been obtained.
*             Set finished flag to 'unfinished', the text to indicate
*             that the table extension header will follow and the class
*             to 'CAT'.

               FINISH = .FALSE.
               TEXT = 'Table Extension Header:-'
               CLASS = 'CAT'

*
*             Move to the extension header.

               CALL FTMRHD (FITUNT, EXTN__CAT3(CIELM), HDUTYP, FITSTT)
               IF (FITSTT .NE. FITOK) THEN
                  STATUS = CAT__ERROR
                  CALL CAT3_FITER ('CAT3_GETXT_RHD', 'Failed to access'/
     :              /' the specified FITS extension header.', FITSTT,
     :              STATUS)
               END IF

*
*             Determine the number of keywords in the header and set
*             the current keyword to 0.  Also, update the state to 'in
*             the extension header'.

               CALL FTGHSP (FITUNT, NKEYWD, KEYADD, FITSTT)
               IF (FITSTT .EQ. FITOK) THEN
                  NUMKY__CAT3(CIELM) = NKEYWD
                  CURKY__CAT3(CIELM) = 0
                  HSTAT__CAT3(CIELM) = CAT3__HSTTE

               ELSE
                  STATUS = CAT__ERROR
                  CALL CAT3_FITER ('CAT3_GETXT_HSP', 'Failed to '/
     :              /'determine the number of keywords in the '/
     :              /'extension header.', FITSTT, STATUS)

               END IF
            END IF

         ELSE IF (HSTATE .EQ. CAT3__HSTTE) THEN

*
*          The current state is 'in the extension header'.  Increment
*          to the next keyword and check if this keyword exists.

            CURKY__CAT3(CIELM) = CURKY__CAT3(CIELM) + 1

            IF (CURKY__CAT3(CIELM) .LE. NUMKY__CAT3(CIELM) ) THEN

*
*             Attempt to get the text for this keyword and proceed if
*             ok.

               CALL FTGREC (FITUNT, CURKY__CAT3(CIELM), KCARD, FITSTT)

               IF (FITSTT .EQ. FITOK) THEN

*
*                Set the finished flag to unfinished, accept the
*                contents of the header record as the text and determine
*                its class.

                  FINISH = .FALSE.
                  TEXT = KCARD
                  CALL CAT3_GTXCL (KCARD, CLASS, STATUS)

               ELSE

*
*                Set the status, report an error and set the return
*                variables.

                  STATUS = CAT__ERROR

                  CALL CAT3_FITER ('CAT3_GETXT_CRE', 'Error getting '/
     :              /'a record from the extension header.', FITSTT,
     :              STATUS)

                  FINISH = .TRUE.
                  TEXT = ' '
                  CLASS = ' '

               END IF

            ELSE

*
*             All the keywords in the extension header have been
*             obtained.  That is, all the textual information has been
*             obtained.  Set the finished flag to 'finished' and set the
*             other return arguments to blank.

               FINISH = .TRUE.
               TEXT = ' '
               CLASS = ' '

            END IF

         ELSE

*
*          The code for the current access to the text information is
*          invalid.  Set the status, report an error and set the return
*          arguments.

            STATUS = CAT__ERROR

            ERRTXT = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Illegal state for access to FITS table '/
     :        /'header text (code: ', ERRTXT, ERRLEN)
            CALL CHR_PUTI (HSTATE, ERRTXT, ERRLEN)
            CALL CHR_PUTC (').', ERRTXT, ERRLEN)

            CALL CAT1_ERREP ('CAT3_GETXT_IST', ERRTXT(1 : ERRLEN),
     :        STATUS)

            FINISH = .TRUE.
            TEXT = ' '
            CLASS = ' '

         END IF

      END IF

      END
