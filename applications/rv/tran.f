      SUBROUTINE TRAN (INBUF,INREC)
*+
*
*  TRAN:  subroutine of RV utility which translates non-printable
*         characters to spaces and converts lowercase to uppercase.
*
*  Given:
*     INBUF     char     record as received
*
*  Returned:
*     INREC     char     record with TABs etc and lowercase eliminated
*
*  n.b.  This implementation is ASCII-dependent !!!
*
*  P T Wallace   Starlink   9 June 1992
*-

      IMPLICIT NONE

      CHARACTER*(*) INBUF,INREC

      INTEGER NUC,I
      CHARACTER C


*  Offset from lowercase set to uppercase   !!! ASCII-dependent !!!
      NUC=ICHAR('a')-ICHAR('A')

*  Initialise output string
      INREC=' '

*  Process each character
      DO I=1,LEN(INBUF)
         C=INBUF(I:I)
         IF (C.LT.' '.OR.C.GT.'~') THEN
            C=' '
         ELSE IF (C.GE.'a'.AND.C.LE.'z') THEN
            C=CHAR(ICHAR(C)-NUC)
         END IF
         INREC(I:I)=C
      END DO

      END
