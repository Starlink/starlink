**==ovrwrt.spg  processed by SPAG 4.54K  at 14:22 on  4 Oct 1996
c
c
c
      SUBROUTINE OVRWRT(LINE,IWHICH)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      INTEGER IWHICH
C*** End of declarations inserted by SPAG
      CHARACTER*(*) LINE
      CHARACTER*79 OUTPUT
      INTEGER LEN
      IF ( IWHICH.EQ.1 ) THEN
         WRITE (6,9001) LINE
      ELSE IF ( IWHICH.EQ.2 ) THEN
         IF ( LEN(LINE).LT.79 ) THEN
            OUTPUT = ' '
            OUTPUT = LINE
            WRITE (6,9002) OUTPUT , CHAR(13)
            WRITE (6,9002) OUTPUT , CHAR(13)
            WRITE (6,9002) OUTPUT , CHAR(13)
         ELSE
            WRITE (6,9002) LINE , CHAR(13)
         END IF
      ELSE IF ( IWHICH.EQ.3 ) THEN
         WRITE (6,9003) LINE
      ELSE
         WRITE (6,9004) LINE , CHAR(13)
         WRITE (6,9002) LINE , CHAR(13)
      END IF
      RETURN
 9001 FORMAT (a)
 9002 FORMAT (a,a1,$)
 9003 FORMAT (a)
 9004 FORMAT (/a,a1,$)
      END
