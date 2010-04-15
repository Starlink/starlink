!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!
!
!   INTEGER FUNCTION SLEN
!
!   IMPORTS:
!     STRING   (CHARACTER)
!
!   RETURNS:
!     SLEN     (INTEGER)   = index of last non-blank character
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       INTEGER FUNCTION SLEN(STRING)
!
!
       IMPLICIT NONE
!
!
       CHARACTER*(*) STRING
!
!
       INTEGER I
!
!
       DO I = LEN(STRING), 1, -1
           SLEN = I
           IF (STRING(I:I).NE.' ') THEN
              IF (ICHAR(STRING(SLEN:SLEN)).NE.0) GOTO10
           ENDIF
       ENDDO
       SLEN = 0
  10   CONTINUE

       RETURN
       END
