*+ SREAD - Read, condition, paraphrase and classify a string.

      SUBROUTINE SREAD (LU, V1, V2, V3, JSTAT)

*
*  - - - - - -
*   S R E A D
*  - - - - - -
*
*  Read, condition, paraphrase and classify a string.
*
*  A string is input from the nominated I/O unit LU (in A format)
*  and, unless end-of-file was detected, three versions of it are
*  created.
*
*  Version 1 (V1) is as input except that spaces replace any
*  non-printing characters (e.g. TABs).
*
*  Version 2 (V2) is the same as version 1 except that any leading
*  spaces or trailing comments are removed.
*
*  Version 3 (V3) is the same as version 2 except that outside
*  "string arguments", lowercase is converted to uppercase.
*
*  Given:
*     LU         i         I/O unit for input
*
*  Returned:
*     V1         c*(*)     string with TABS etc eliminated
*     V2         c*(*)     string left justified and without comments
*     JSTAT      i         status:  -1 = end-of-file
*                                    0 = not a comment
*                                   +1 = comment
*
*  Notes:
*
*  1)  "String arguments" are groups of characters enclosed by
*      pairs of either single or double quotes.  The contents
*      are protected against conversion to uppercase.  The delimiters
*      are ultimately removed by means of the GETSTR routine.
*
*  2)  A '!' character is only interpreted as start of comment
*      if it is not within a string argument.
*
*  3)  If end-of-file is detected, spaces are returned for V1,
*      V2 and V3.
*
*  4)  The algorithm uses several characters which are outside
*      the ANSI FORTRAN 77 set (exclamation point, double quote,
*      tilde, lowercase).  Different machine types may need a
*      rewrite.
*
*  5)  This subprogram was suggested by the RDNEXT routine of
*      Russell Owen (UW/ARC).
*
*  Called:   CHR_UCASE
*
*  P.T.Wallace   Starlink   27 April 1988
*  Malcolm J. Currie   1988 Sep 9  - changed UC to CHR_UCASE for
*                                    KAPPA use
*
*+

      IMPLICIT NONE

      INTEGER LU
      CHARACTER*(*) V1,V2,V3
      INTEGER JSTAT

      INTEGER L1,L2,I1,I2
      CHARACTER CDELIM,C
      LOGICAL COMENT



*  Obtain the string lengths
      L1=LEN(V1)
      L2=LEN(V2)

*  Initialise second pointer
      I2=0

*  Preset the first two strings to spaces
      V1=' '
      V2=' '

*  Reset the "string argument has started" flag
      CDELIM=' '

*  Reset the "comment has started" flag
      COMENT=.FALSE.

*  Preset the status to EOF
      JSTAT=-1

*  Read the string
      READ (LU,'(A)',END=9999) V1

*  Not EOF:  preset the status to "comment"
      JSTAT=1

*  Step along the raw input string
      DO I1=1,L1

*     Next character
         C=V1(I1:I1)

*     Replace non-printing characters with spaces
         IF (C.LT.' '.OR.C.GT.'~') C=' '

*     "String argument" delimiter character?
         IF (.NOT.COMENT.AND.
     :       C.EQ.'"'.OR.C.EQ.'''') THEN

*        Yes:  leading or trailing?
            IF (CDELIM.EQ.' ') THEN

*           Leading:  remember
               CDELIM=C

            ELSE IF (C.EQ.CDELIM) THEN

*           Trailing:  remember
               CDELIM=' '

            END IF

         END IF

*     Store character in first string
         V1(I1:I1)=C

*     Start of comment?
         IF (CDELIM.EQ.' '.AND.C.EQ.'!') COMENT=.TRUE.

*     Are we within a comment?
         IF (.NOT.COMENT) THEN

*        No:  leading space?
            IF (I2.NE.0.OR.C.NE.' ') THEN

*           No:  reset "comment" flag
               JSTAT=0

*           Store character in string 2 unless already filled
               IF (I2.LT.L2) THEN
                  I2=I2+1
                  V2(I2:I2)=C
               END IF
            END IF
         END IF
      END DO

*  Generate uppercase version and exit
 9999 CONTINUE
      V3=V2
      CALL CHR_UCASE(V3)

      END
