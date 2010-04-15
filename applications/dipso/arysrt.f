!
!   ARYSRT - sorts rows of a real matrix into ascending order
!            of an index column (explicit replacement for M01AEF)
!
!   Imports:
!     ARY    real array(NR,n)  n>=NC
!            ARY  is updated, the section bounded by NR & NC being
!            sorted on column IC
!     NR     integer number of rows to be sorted
!     NC     integer number of columns to be sorted
!     IC     integer index column for sort
!     CMD    invoking command character string
!     OK     logical success flag
!
       SUBROUTINE ARYSRT
     : (ARY, NR, NC, IC, CMD, OK)

       IMPLICIT NONE
       INTEGER NR, NC, IC
       REAL ARY(NR,NC)
       CHARACTER*(*) CMD
       LOGICAL OK

       INTEGER I, J, SLEN
       REAL TEMP
       LOGICAL SORTED

       OK = .TRUE.
       IF (IC.GT.NC) THEN
          WRITE (*,
     :    '(''   '',A,'':  sort error in ARYSRT'')') CMD(1:SLEN(CMD))
          OK = .FALSE.
          GOTO 300
       ENDIF

   10  CONTINUE
       SORTED = .TRUE.

       DO 200 I = 2, NR
          IF (ARY(I,IC).LT.ARY(I-1,IC)) THEN
             SORTED = .FALSE.
             DO 100 J = 1, NC
                TEMP = ARY(I,J)
                ARY(I,J) = ARY(I-1,J)
                ARY(I-1,J) = TEMP
  100        CONTINUE
          ENDIF
  200  CONTINUE

       IF (.NOT.SORTED) GOTO 10

  300  CONTINUE

       END
