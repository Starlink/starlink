      SUBROUTINE CAP_PRLIN (BUFFER, LINE, STATE, PRSOK, STATUS)
*+
*  Name:
*     CAP_PRLIN
*  Purpose:
*     Parse a single line from the graphics translation file.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PRLIN (BUFFER, LINE; STATE, PRSOK; STATUS)
*  Description:
*     Parse a single line from the graphics translation file.
*  Arguments:
*     BUFFER  =  CHARACTER*(*) (Given)
*        Buffer containing the line.
*     LINE  =  INTEGER (Given)
*        Number of the line in the graphics translation file (for
*        use in parser error messages).
*     STATE  =  INTEGER (Given and Returned)
*        State of the parser: in or out of an IF block.
*     PRSOK  =  LOGICAL (Given and Returned)
*        Flag indicating whether the line parsed ok (= .TRUE. if it
*        did).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Take a copy of the line (which may be modified).
*     Remove any spaces.
*     Convert the copy to upper case.
*     If the line is an 'IF' then
*       If there is space for another IF block then
*         Increment the number of IF blocks.
*         Set to the first clause in the block.
*         Extract and save the expression.
*         Set all the values to undefined.
*         Set the state to 'IN IF'.
*       else
*         Set the termination flag.
*         Report an error.
*       end if
*     else the line is an 'ELSE IF' then
*       If there is space for another IF clause in the current block then
*         Increment the number of IF clauses in the current block.
*         Extract and save the expression.
*         Set all the values to undefined.
*       else
*         Set the termination flag.
*         Report an error.
*       end if
*     else the line is an 'ELSE' then
*       If there is space for another IF clause in the current block then
*         Increment the number of IF clauses in the current block.
*         Set the expression to 'default'.
*         Set all the values to undefined.
*       else
*         Set the termination flag.
*         Report an error.
*       end if
*     else the line is 'END IF' then
*       Set the state to 'outside any IF blocks'.
*     else (the line must be an 'item = value')
*       Attempt to decode the value for the given line.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     2/8/96  (ACD): Original version.
*     11/8/96 (ACD): First stable version.
*     6/6/97  (ACD): Changed the subroutine prefix from CIO to CAP.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'CIO_PAR'           ! CIO parametric constants.
*  Global Variables:
      INCLUDE 'CIO_CMN'           ! CIO common block.
*  Arguments Given:
      CHARACTER
     :  BUFFER*(*)
      INTEGER
     :  LINE
*  Arguments Given and Returned:
      INTEGER
     :  STATE
      LOGICAL
     :  PRSOK
*  Status:
      INTEGER STATUS          ! Global status.
*  Local Constants:
      INTEGER MAXWRD          ! Maximum permitted no. of words in a line.
      PARAMETER (MAXWRD = 40)
*  Local Variables:
      CHARACTER
     :  WRKTYP*(CIO__SZREC),  ! Copy of BUFFER for finding type of line.
     :  WORDS(MAXWRD)*(CIO__SZREC)   ! Individual words in the line.
      INTEGER
     :  NUMWRD,         ! Number of words in the line.
     :  START(MAXWRD),  ! Start position of each word.
     :  STOP(MAXWRD),   ! Stop     "     "   "    "  .
     :  LSTAT,          ! Local status.
     :  CURCLS          ! Number of the current clause.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Take a copy of the buffer (which will be used to determine the
*       type of the line), remove any embedded spaces and convert it
*       to upper case.

         WRKTYP = BUFFER
         CALL CHR_RMBLK (WRKTYP)
         CALL CHR_UCASE (WRKTYP)

*
*       Check whether the line is an IF.  If so then start an new block
*       if there is space.

         IF (WRKTYP(1 : 2) .EQ. 'IF') THEN
            IF (NIFB__CIO .LT. CIO__MXIFB) THEN
               NIFB__CIO = NIFB__CIO + 1
               NCLS__CIO(NIFB__CIO) = 1

               CALL CHR_DCWRD (BUFFER, MAXWRD, NUMWRD, START, STOP,
     :           WORDS, LSTAT)

               IF (NUMWRD .GE. 2) THEN
                  CRIT__CIO(NIFB__CIO, 1) =
     :              BUFFER(START(2) : STOP(NUMWRD) )

                  SYMB__CIO(NIFB__CIO, 1) = CIO__NULL
                  COLR__CIO(NIFB__CIO, 1) = CIO__NULL
                  UNIT__CIO(NIFB__CIO, 1) = CIO__NULL
                  SIZ1__CIO(NIFB__CIO, 1) = ' '
                  SIZ2__CIO(NIFB__CIO, 1) = ' '
                  SIZ3__CIO(NIFB__CIO, 1) = ' '
                  SIZ4__CIO(NIFB__CIO, 1) = ' '
                  LABL__CIO(NIFB__CIO, 1) = ' '

                  STATE = CIO__INIB

               ELSE
                  CALL CAP_PRERR (LINE, 'missing condition for IF.',
     :              STATUS)

               END IF
            ELSE
               STATUS = SAI__ERROR

               CALL MSG_SETI ('MXIFB', CIO__MXIFB)
               CALL ERR_REP ('CIO_PRLIN_MXIFB', 'too many IF blocks '/
     :           /'(^MXIFB permitted).', STATUS)

            END IF

*
*       Check whether the line is an ELSE IF.  If so then start a new
*       clause if there is space.

         ELSE IF (WRKTYP(1 : 6) .EQ. 'ELSEIF') THEN
            CURCLS = NCLS__CIO(NIFB__CIO)
            IF (CURCLS .LT. CIO__MXCLS) THEN
               CURCLS = CURCLS + 1
               NCLS__CIO(NIFB__CIO) = CURCLS

               CALL CHR_DCWRD (BUFFER, MAXWRD, NUMWRD, START, STOP,
     :           WORDS, LSTAT)

*
*             Note that the following code handles the two cases where
*             there either is or is not a space between the 'IF' and
*             the 'ELSE'.

               IF (WORDS(1) .EQ. 'ELSEIF')THEN
                  IF (NUMWRD .GE. 2) THEN
                     CRIT__CIO(NIFB__CIO, CURCLS) =
     :                 BUFFER(START(2) : STOP(NUMWRD) )
                  ELSE
                     PRSOK = .FALSE.
                  END IF
               ELSE
                  IF (NUMWRD .GE. 3) THEN
                     CRIT__CIO(NIFB__CIO, CURCLS) =
     :                 BUFFER(START(3) : STOP(NUMWRD) )
                  ELSE
                     PRSOK = .FALSE.
                  END IF
               END IF

               IF (PRSOK) THEN
                  SYMB__CIO(NIFB__CIO, CURCLS) = CIO__NULL
                  COLR__CIO(NIFB__CIO, CURCLS) = CIO__NULL
                  UNIT__CIO(NIFB__CIO, CURCLS) = CIO__NULL
                  SIZ1__CIO(NIFB__CIO, CURCLS) = ' '
                  SIZ2__CIO(NIFB__CIO, CURCLS) = ' '
                  SIZ3__CIO(NIFB__CIO, CURCLS) = ' '
                  SIZ4__CIO(NIFB__CIO, CURCLS) = ' '
                  LABL__CIO(NIFB__CIO, CURCLS) = ' '

               ELSE
                  CALL CAP_PRERR (LINE, 'missing condition for ELSE '/
     :              /'IF.', STATUS)

               END IF
            ELSE
               STATUS = SAI__ERROR

               CALL MSG_SETI ('MXCLS', CIO__MXCLS)
               CALL ERR_REP ('CIO_PRLIN_MXCL', 'too many clauses in '/
     :           /'IF statement (^MXIFB permitted).', STATUS)

            END IF

*
*       Check whether the line is an ELSE.  If so then start a new
*       clause if there is space.

         ELSE IF (WRKTYP(1 : 4) .EQ. 'ELSE'  .AND.
     :            WRKTYP(5 : 6) .NE. 'IF') THEN
            CURCLS = NCLS__CIO(NIFB__CIO)
            IF (CURCLS .LT. CIO__MXCLS) THEN
               CURCLS = CURCLS + 1
               NCLS__CIO(NIFB__CIO) = CURCLS

               CRIT__CIO(NIFB__CIO, CURCLS) = '<default>'

               SYMB__CIO(NIFB__CIO, CURCLS) = CIO__NULL
               COLR__CIO(NIFB__CIO, CURCLS) = CIO__NULL
               UNIT__CIO(NIFB__CIO, CURCLS) = CIO__NULL
               SIZ1__CIO(NIFB__CIO, CURCLS) = ' '
               SIZ2__CIO(NIFB__CIO, CURCLS) = ' '
               SIZ3__CIO(NIFB__CIO, CURCLS) = ' '
               SIZ4__CIO(NIFB__CIO, CURCLS) = ' '
               LABL__CIO(NIFB__CIO, CURCLS) = ' '

            ELSE
               STATUS = SAI__ERROR

               CALL MSG_SETI ('MXCLS', CIO__MXCLS)
               CALL ERR_REP ('CIO_PRLIN_MXCLD', 'too many clauses in '/
     :           /'IF statement (^MXIFB permitted).', STATUS)

            END IF

*
*       Check whether the line is an 'END IF'.  If so then set the state
*       to 'out of IF block'.

         ELSE IF (WRKTYP(1 : 5) .EQ. 'ENDIF') THEN
            STATE = CIO__OUTIB

*
*       All the other possibilities having been examined, the line
*       must be setting the value of some item.  Attempt to decode and
*       set the value.

         ELSE
            CALL CAP_PRITM (BUFFER, STATE, LINE, PRSOK, STATUS)

         END IF

      END IF

      END
