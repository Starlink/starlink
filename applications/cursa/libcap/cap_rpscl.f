      SUBROUTINE CAP_RPSCL (CI, EXPR, STATUS)
*+
*  Name:
*     CAP_RPSCL
*  Purpose:
*     Substitute 'ASCALE' with corresponding 'SCALE' in expression.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_RPSCL (CI; EXPR; STATUS)
*  Description:
*     A string containing an expression is examined and any 'ASCALE'
*     function found is replaced with the corresponding 'SCALE'
*     function.  If no 'ASCALE' is found the expression is returned
*     unchanged.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier for the parent catalogue of the expression.
*     EXPR  =  CHARACTER*(*) (Given and Returned)
*        The expression to be checked and modified.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Take a working copy of the expression.
*     Convert the expression into upper case.
*     Replace any brackets and commas with spaces.
*     Split the expression into its constituent words.
*     Check if one of the words is 'ASCALE'
*     If so then
*       Check that all the arguments for 'ASCALE' are present.
*       If so then
*         Attempt to get an identifier for the column to be scaled.
*         If ok then
*           Determine the number of rows.
*           Attempt to find the minimum and maximum values of the
*           column.
*           If ok then
*             Generate the corresponding 'SCALE' expression.
*           else
*             Set the status.
*             Report error: failed to find the range of the column.
*           end if
*         else
*           Report error: failed to get identifier for column.
*         end if
*       else
*         Set the status.
*         Report error: wrong number of arguments for 'ASCALE'.
*       end if
*     end if
*  Implementation Deficiencies:
*     The routine only handles the cases where the expression contains
*     one or zero occurences of 'ASCALE'; expressions containing multiple
*     occurrences of 'ASCALE' are treated incorrectly.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     15/8/96 (ACD): Original version.
*     6/6/97  (ACD): Changed the subroutine prefix from CIO to CAP.
*     7/6/00  (ACD): Modified to handle catalogues with 1 or 0 rows.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
*  Arguments Given:
      INTEGER
     :  CI
*  Arguments Given and Returned:
      CHARACTER
     :  EXPR*(*)
*  Status:
      INTEGER STATUS    ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Constants:
      INTEGER MAXWRD    ! Maximum permitted no. of words in expression.
      PARAMETER (MAXWRD = 30)

      INTEGER MINRNG    ! Minimum permitted range.
      PARAMETER (MINRNG = 1.0D-8)
*  Local Variables:
      CHARACTER
     :  WEXPR*(CAT__SZEXP),          ! Working copy of EXPR.
     :  WORDS(MAXWRD)*(CAT__SZEXP),  ! Words in the expression.
     :  ERRTXT*75                    ! Text of error message.
      INTEGER
     :  LEXPR,    ! Length of EXPR   (excl. trail. blanks).
     :  LWEXPR,   !   "    "  WEXPR  ( "  .   "  .   "   ).
     :  ERRLEN,   !   "    "  ERRTXT ( "  .   "  .   "   ).
     :  LOOP,     ! Loop index.
     :  NUMWRD,   ! Number of words in the expression.
     :  START(MAXWRD),  ! Start position of each word in expression.
     :  STOP(MAXWRD),   ! Stop     "     "   "    "   "      "     .
     :  LSTAT,    ! Local status.
     :  NSCALE,   ! Position of 'ASCALE' in words.
     :  FI,       ! Column identifier.
     :  ROWS,     ! Number of rows in the catalogue.
     :  ROW       ! Current row number.
      LOGICAL
     :  MINFND,   ! Flag; has maximum been found?
     :  MAXFND,   !  "  ;  "  minimum  "     "  ?
     :  NULFLG    ! Flag; is the current field null?
      DOUBLE PRECISION
     :  MINVAL,   ! Maximum value of column.
     :  MAXVAL,   ! Minimum   "   "    "   .
     :  AVGVAL,   ! Average   "   "    "   .
     :  VALUE     ! Value of the current field.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Take a working copy of the expression and convert it into upper
*       case.

         WEXPR = EXPR
         CALL CHR_UCASE (WEXPR)

*
*       Replace any brackets and commas with spaces.

         IF (WEXPR .NE. ' ') THEN
            LWEXPR = CHR_LEN(EXPR)
         ELSE
            LWEXPR = 1
         END IF

         DO LOOP = 1, LWEXPR
            IF (WEXPR(LOOP : LOOP) .EQ. '('  .OR.
     :          WEXPR(LOOP : LOOP) .EQ. ')'  .OR.
     :          WEXPR(LOOP : LOOP) .EQ. ',') THEN
               WEXPR(LOOP : LOOP) = ' '
            END IF
         END DO

*
*       Split the string into its constituent words.

         CALL CHR_DCWRD (WEXPR, MAXWRD, NUMWRD, START, STOP, WORDS,
     :     LSTAT)

*
*       Check if one of the words is 'ASCALE' and if so proceed.

         NSCALE = 0

         DO LOOP = 1, NUMWRD
            IF (WORDS(LOOP) .EQ. 'ASCALE') THEN
               NSCALE = LOOP
            END IF
         END DO

         IF (NSCALE .GT. 0) THEN

*
*          Check that all the arguments for 'ASCALE' are present.

            IF (NUMWRD .GE. NSCALE+3) THEN

*
*             Attempt to get an identifier for the column to be scaled
*             and proceed if ok.  This column must be the first argument
*             of 'ASCALE'.

               CALL CAT_TIDNT (CI, WORDS(NSCALE+1), FI, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN

*
*                Determine the number of rows.

                  CALL CAT_TROWS (CI, ROWS, STATUS)

*
*                Attempt to find the minimum and maximum values of the
*                column.  The special case of zero rows is handled
*                separately and explicitly.

                  IF (ROWS .GE. 1) THEN
                     MINFND = .FALSE.
                     MAXFND = .FALSE.

                     MINVAL = 0.0D0
                     MAXVAL = 0.0D0

                     DO ROW = 1, ROWS
                        CALL CAT_RGET (CI, ROW, STATUS)
                        CALL CAT_EGT0D (FI, VALUE, NULFLG, STATUS)

                        IF (.NOT. NULFLG) THEN
                           IF (MINFND) THEN
                              IF (VALUE .LT. MINVAL) THEN
                                 MINVAL = VALUE
                              END IF
                           ELSE
                              MINVAL = VALUE
                              MINFND = .TRUE.
                           END IF

                           IF (MAXFND) THEN
                              IF (VALUE .GT. MAXVAL) THEN
                                 MAXVAL = VALUE
                              END IF
                           ELSE
                              MAXVAL = VALUE
                              MAXFND = .TRUE.
                           END IF

                        END IF
                     END DO

                     IF (MINFND  .AND.  MAXFND) THEN
                        IF (ABS(MAXVAL-MINVAL) .LT. MINRNG) THEN
                           AVGVAL = (MAXVAL + MINVAL) / 2.0D0

                           MINVAL = AVGVAL * 9.5D-1
                           MAXVAL = AVGVAL * 1.05D0
                        END IF
                     END IF

                  ELSE
                     MINFND = .FALSE.
                     MAXFND = .FALSE.
                  END IF

                  IF (.NOT. MINFND  .OR.  .NOT. MAXFND) THEN
                     MINVAL = -1.0D0
                     MAXVAL = 1.0D0
                  END IF

*
*                Proceed ok.

                  IF (STATUS .EQ. SAI__OK) THEN

*
*                   Generate the 'SCALE' expression corresponding to
*                   the given 'ASCALE' expression.

                     WEXPR = ' '
                     LWEXPR = 0

                     IF (NSCALE .GT. 1) THEN
                        CALL CHR_PUTC (EXPR(1 : STOP(NSCALE-1)),
     :                    WEXPR, LWEXPR)
                     END IF

                     CALL CHR_PUTC ('scale(', WEXPR, LWEXPR)
                     CALL CHR_PUTC (
     :                 EXPR(START(NSCALE+1) : STOP(NSCALE+1) ),
     :                 WEXPR, LWEXPR)

                     CALL CHR_PUTC (',', WEXPR, LWEXPR)
                     CALL CHR_PUTD (MINVAL, WEXPR, LWEXPR)

                     CALL CHR_PUTC (',', WEXPR, LWEXPR)
                     CALL CHR_PUTD (MAXVAL, WEXPR, LWEXPR)

                     CALL CHR_PUTC (',', WEXPR, LWEXPR)
                     CALL CHR_PUTC (
     :                 EXPR(START(NSCALE+2) : STOP(NSCALE+2) ),
     :                 WEXPR, LWEXPR)

                     CALL CHR_PUTC (',', WEXPR, LWEXPR)
                     CALL CHR_PUTC (
     :                 EXPR(START(NSCALE+3) : STOP(NSCALE+3) ),
     :                 WEXPR, LWEXPR)

                     CALL CHR_PUTC (')', WEXPR, LWEXPR)

                     IF (NUMWRD .GT. NSCALE+3) THEN
                        CALL CHR_PUTC (
     :                    EXPR(START(NSCALE+4) : STOP(NUMWRD) ),
     :                    WEXPR, LWEXPR)
                     END IF

                     EXPR = ' '
                     EXPR = WEXPR
                  ELSE

*
*                   Set the status (if it is not already set) and report
*                   error: failed to find the range of the column.

                     IF (STATUS .EQ. SAI__OK) THEN
                        STATUS = SAI__ERROR
                     END IF

                     ERRTXT = ' '
                     ERRLEN = 0

                     CALL CHR_PUTC ('Unable to determine range for '/
     :                 /'ASCALE: ', ERRTXT, ERRLEN)

                     LEXPR = CHR_LEN(EXPR)
                     IF (LEXPR .LE. 30) THEN
                        CALL CHR_PUTC (EXPR(1 : LEXPR), ERRTXT, ERRLEN)
                     ELSE
                        CALL CHR_PUTC (EXPR(1 : 27), ERRTXT, ERRLEN)
                        CALL CHR_PUTC ('...', ERRTXT, ERRLEN)
                     END IF

                     CALL ERR_REP ('CAP_RPSCL_RNG', ERRTXT(1 : ERRLEN),
     :                 STATUS)
                  END IF

               ELSE

*
*                Report error: failed to get identifier for column.

                  ERRTXT = ' '
                  ERRLEN = 0

                  CALL CHR_PUTC ('Unknown column in ASCALE: ',
     :              ERRTXT, ERRLEN)

                  LEXPR = CHR_LEN(EXPR)
                  IF (LEXPR .LE. 30) THEN
                     CALL CHR_PUTC (EXPR(1 : LEXPR), ERRTXT, ERRLEN)
                  ELSE
                     CALL CHR_PUTC (EXPR(1 : 27), ERRTXT, ERRLEN)
                     CALL CHR_PUTC ('...', ERRTXT, ERRLEN)
                  END IF

                  CALL ERR_REP ('CAP_RPSCL_UCL', ERRTXT(1 : ERRLEN),
     :              STATUS)
               END IF

            ELSE

*
*             Set the status and report error: wrong number of arguments
*             for 'ASCALE'.

               STATUS = SAI__ERROR

               ERRTXT = ' '
               ERRLEN = 0

               CALL CHR_PUTC ('Wrong no. of arguments for ASCALE: ',
     :           ERRTXT, ERRLEN)

               LEXPR = CHR_LEN(EXPR)
               IF (LEXPR .LE. 30) THEN
                  CALL CHR_PUTC (EXPR(1 : LEXPR), ERRTXT, ERRLEN)
               ELSE
                  CALL CHR_PUTC (EXPR(1 : 27), ERRTXT, ERRLEN)
                  CALL CHR_PUTC ('...', ERRTXT, ERRLEN)
               END IF

               CALL ERR_REP ('CAP_RPSCL_WAG', ERRTXT(1 : ERRLEN),
     :           STATUS)
            END IF
         END IF

      END IF

      END
