**==PUTEXT.FOR
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE PUTEXT(TEXT,LLEN,REAL)
!
       IMPLICIT NONE
!
       CHARACTER*80 TEXT
       REAL REAL
       INTEGER LLEN
       INTEGER SLEN
!
!
       TEXT = ' '
       WRITE (TEXT,'(1PG15.5)') REAL
       CALL SSTRIP(TEXT)
       LLEN = SLEN(TEXT)
!
!
       RETURN
       END
