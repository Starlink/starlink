       SUBROUTINE ITOCHR (I, INTSTR, LSTR, OK)

       IMPLICIT NONE
       INTEGER I, J
       LOGICAL OK
       INTEGER LSTR, IERR
       CHARACTER*10 INTSTR

       INTSTR = ' '
       OK = .TRUE.

*
*   Put number into string, left-justified
       WRITE (INTSTR(1:),'(I10)',IOSTAT=IERR) I
       IF (IERR.NE.0) THEN
          OK = .FALSE.
          GOTO 9000
       ENDIF

       LSTR = LEN(INTSTR)
       IF (INTSTR.NE.' ') THEN
  100     CONTINUE
          IF (INTSTR(1:1).EQ.' ') THEN
             DO J = 1, LSTR-1
                INTSTR( J : J ) = INTSTR( J+1 : J+1 )
             END DO
             INTSTR(LSTR:) = ' '
             LSTR = LSTR - 1
             GOTO 100
          ENDIF
       ENDIF

 9000  CONTINUE
       END
