******************************************************************
*
*   Subroutine IWRITE
*
*   Outputs integer numbers embedded in text
*
*   Inputs:
*     S1   preceding text (may be null)
*     I1   integer to be output
*     S2   following text (may be null)
*     IO   integer output unit
*     OK   Logical, .TRUE. on success
*
*   Calls integer function SLEN to determine `active' length
*   of string
*
****************************************************************

       SUBROUTINE IWRITE
     : (S1, I1, S2, IO, OK)

       IMPLICIT NONE

       CHARACTER*(*) S1
       CHARACTER*(*) S2

       INTEGER I1, IO

       LOGICAL OK

*

       INTEGER SLEN, LSTR, IERR, J

       CHARACTER*10 INTSTR
       CHARACTER*132 OUTSTR

*

       INTSTR = ' '
       OK = .TRUE.

*
*   Put number into string, left-justified
*

       WRITE (INTSTR(1:),'(I10)',IOSTAT=IERR) I1
       IF (IERR.NE.0) THEN
          WRITE (IO,
     :    '(''   Output error in routine IWRITE'',A)') 7
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

*
*   Output
*
       OUTSTR =
     : S1(1:SLEN(S1))//' '//INTSTR(1:SLEN(INTSTR))//' '//S2(1:SLEN(S2))
       WRITE (IO,'(A)') OUTSTR

*

 9000  CONTINUE
       END
