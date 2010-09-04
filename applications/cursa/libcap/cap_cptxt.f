      SUBROUTINE CAP_CPTXT (CIIN, CIOUT, TXTFLG, STATUS)
*+
*  Name:
*     CAP_CPTXT
*  Purpose:
*     Copy header text from the input to the output catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CPTXT (CIIN, CIOUT, TXTFLG; STATUS)
*  Description:
*     Copy header text from the input to the output catalogue.  If just
*     comments are being copied an attempt is made to remove information
*     identifying the record as a comment from the start of the
*     comment.
*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier for the input catalogue.
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     TXTFLG  =  CHARACTER*(*) (given)
*        Flag indicating the textual information to be copied, coded
*        as follows:
*        A - all (that is, generate a complete copy of the original
*            header as comments),
*        C - just copy the genuine comments (that is, COMMENTS and
*            HISTORY keywords in the case of FITS tables).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Get the maximum line size for the output catalogue.
*     Reset access to the input catalogue textual information.
*     Do while there is more text to copy
*       Attempt to get the next line of text from the input catalogue.
*       If (the text is not finished and the status is ok) then
*         If all the text is being copied, or the class of the line
*         indicates that it is a comment then
*           If only comments are being copied then
*             Attempt to remove any text at the start of the line
*             indicating its type.
*           end if
*           Determine the length of the line of text.
*           If the line can fit on a single output line then
*             Output the line.
*           else
*             Split the line in two and output it as two lines.
*           end if
*         end if
*       end if
*       If the text from the input catalogue is finished then
*         Set the termination flag.
*       end if
*       If the status is not ok then
*         Set the termination flag.
*         Report an error.
*       end if
*     end do
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     28/9/94  (ACD): Original version.
*     29/9/94  (ACD): First stable version.
*     26/8/96  (ACD): Added option to copy either all the header text
*        or just the genuine comments.
*     10/12/96 (ACD): Modified to handle KAPPA format STLs.
*     11/11/97 (ACD): Fixed a bug in copying blank comments.
*     11/10/99 (ACD): Modified to handle the AST class.
*     17/3/00  (ACD): Reset access to the input catalogues textual
*        information before beginning the copy.
*     19/4/00  (ACD): Prevented the addition of spurious spaces when
*        at the start of the line when copying AST lines.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants.
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants.
*  Arguments Given:
      INTEGER
     :  CIIN,
     :  CIOUT
      CHARACTER
     :  TXTFLG*(*)
*  Status:
      INTEGER STATUS       ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER MAXWRD       ! Max. permitted no. of words on a line.
      PARAMETER (MAXWRD = 10)
*  Local Variables:
      INTEGER
     :  SZOLIN,    ! Max. permitted size of line in output cat.
     :  LTEXT,     ! Length of TEXT (excl. trail. blanks).
     :  NUMWRD,    ! Number of words in the current line.
     :  START(MAXWRD),     ! Start position of each word.
     :  STOP(MAXWRD),      ! Stop     "     "   "    "  .
     :  LSTART,    ! Start position for copying text.
     :  LSTAT      ! Local status.
      CHARACTER
     :  CLASS*10,  ! Class of current line of text (from input cat.).
     :  TEXT*(CAT__SZTXL), ! Current line of text.
     :  WORDS(MAXWRD)*10,  ! Words in the current line.
     :  WTEXT*200  ! Working copy of the current line of text.
      LOGICAL
     :  FINISH,    ! Flag: last line of text obtained?
     :  MORE       ! Flag: More lines of text to process?
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Get the maximum line size for the output catalogue.

         CALL CAT_SZTXT (CIOUT, 'WRITE', SZOLIN, STATUS)

*
*       Reset access to the input catalogue textual information.

         CALL CAT_RSTXT (CIIN, STATUS)

*
*       Copy the lines of header text.

         MORE = .TRUE.

         DO WHILE (MORE)

*
*          Attempt to get the next line of text from the input
*          catalogue and proceed if the status is ok and the input
*          of text is not finished.

            TEXT = ' '
            CALL CAT_GETXT (CIIN, FINISH, CLASS, TEXT, STATUS)
C           print2000, finish, class, text
C2000       format(3x, 'finish, class: ', l5, 3x, a, 3x, 'text:-' /
C    :        3x, ':', a, ':' )

            IF (STATUS .EQ. SAI__OK  .AND.  .NOT. FINISH) THEN

*
*             Proceed to copy the line if either all the text is
*             is to be copied or the CLASS of the line indicates that
*             it is a genuine comment.
*
*             The values of CLASS considered to constitute a comment
*             vary for the different catalogue types.  The details are
*             as follows.
*
*             SCAR/ADC:  CATNOTE, ADCNOTE
*             FITS:      COMMENT, HISTORY, AST
*             CHI/HDS:   (text information not supported)
*             STL:       COMMENT, AST

               IF (TXTFLG(1 : 1) .EQ. 'A'  .OR.
     :             CLASS .EQ. 'CATNOTE'  .OR.
     :             CLASS .EQ. 'ADCNOTE'  .OR.
     :             CLASS .EQ. 'COMMENT'  .OR.
     :             CLASS .EQ. 'HISTORY'  .OR.
     :             CLASS .EQ. 'AST') THEN

*
*                If only comments are being copied then attempt to
*                remove any text at the beginning of the line which
*                indicates that it is a comment.

                  IF (TXTFLG( 1 : 1) .EQ. 'C') THEN
                     IF (TEXT .NE. ' ') THEN
                        LTEXT = CHR_LEN(TEXT)

                        CALL CHR_DCWRD (TEXT, MAXWRD, NUMWRD,
     :                    START, STOP, WORDS, LSTAT)
                        CALL CHR_UCASE (WORDS(1) )

                        IF (WORDS(1) .EQ. 'C'  .OR.
     :                      WORDS(1) .EQ. 'A'  .OR.
     :                      WORDS(1) .EQ. 'COMMENT'  .OR.
     :                      WORDS(1) .EQ. 'HISTORY'  .OR.
     :                      WORDS(1) .EQ. 'T'  .OR.
     :                      WORDS(1) .EQ. '#T') THEN
                           IF (NUMWRD .GT. 1) THEN
                              WTEXT = ' '

                              IF (CLASS .EQ. 'AST') THEN
                                 LSTART = START(2)
                              ELSE
                                 LSTART = STOP(1)+2
                              END IF

                              LSTART = MIN(LSTART, LTEXT)
                              WTEXT = TEXT(LSTART : LTEXT)

                              TEXT = ' '
                              TEXT = WTEXT
                           ELSE
                              TEXT = ' '
                           END IF
                        END IF
                     END IF
                  END IF

*
*                Determine the length of the line of text.

                  IF (TEXT .NE. ' ') THEN
                     LTEXT = CHR_LEN(TEXT)
                  ELSE
                     LTEXT = 1
                  END IF

*
*                If the line of text will fit on a single output line
*                then output it as a single line.  Otherwise split it
*                and output it as two lines.

C                 print2001, ltext, text(1 : ltext)
C2001             format(1x, 'line written, ', i4, 'characters:' /
C    :              3x, ':', a, ':')

                  IF (LTEXT .LE. SZOLIN) THEN
                     CALL CAT_PUTXT (CIOUT, 'COMMENT', TEXT(1 : LTEXT),
     :                 STATUS)
                  ELSE
                     CALL CAT_PUTXT (CIOUT, 'COMMENT', TEXT(1 : SZOLIN),
     :                 STATUS)
                     CALL CAT_PUTXT (CIOUT, 'COMMENT', '  '/
     :                 /TEXT(SZOLIN+1 : LTEXT), STATUS)
                  END IF

               END IF
            END IF

*
*          If the last line of text from the input catalogue has been
*          read then set the termination flag.

            IF (FINISH) THEN
               MORE = .FALSE.
            END IF

*
*          If any error has occurred then set the termination flag and
*          report an error.

            IF (STATUS .NE. SAI__OK) THEN
               MORE = .FALSE.

               CALL ERR_REP ('CAP_PUTXT_ERR', 'Error copying text.',
     :           STATUS)
            END IF

         END DO

      END IF

      END
