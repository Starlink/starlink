      SUBROUTINE HELP(CPARAM,IO_LU,TRANS,OUT_LU,IN_LU)
C+
C
C Subroutine: 
C
C     H E L P
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C CPARAM (<), IO_LU (<), TRANS (<), OUT_LU (<), IN_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C
C
C Interactive help routine. This command reads through the polmap.hlp
C file to find the command.
C
C
C
C-
C
      IMPLICIT NONE
      CHARACTER*(*) CPARAM
      CHARACTER*(*) TRANS
      INTEGER OUT_LU,IN_LU
      INTEGER IO_LU
      INTEGER SP
      LOGICAL FOUND
      CHARACTER*79 HELPSTR,HELPSTR2,FILENAME
      CHARACTER*1 DUMMY
      INTEGER I,J
C
      IF (CPARAM.EQ.' ') THEN
       CALL WR_ERROR('Use HELP COMMANDS for a list',OUT_LU)
       CALL WR_ERROR('Or HELP <command> for details',OUT_LU)
       GOTO 999
      ENDIF
C
      I=INDEX(TRANS,' ')
      FILENAME=TRANS(1:(I-1))//'/polmap.hlp'
      HELPSTR=' '
      OPEN(UNIT=IO_LU,FILE=FILENAME,
     &STATUS='OLD',ERR=666)
C
      IF (CPARAM.EQ.'commands') THEN
       J=0
       DO WHILE (HELPSTR.NE.'HELPEND')
        READ(IO_LU,'(A)') HELPSTR
        IF (HELPSTR(1:8).EQ.'Command:') THEN
         SP=INDEX(HELPSTR,':')
         WRITE(OUT_LU,'(1X,A)') HELPSTR((SP+2):)
         J=J+1
        ENDIF
        IF (J.EQ.20) THEN
          WRITE(OUT_LU,*) ' '
          WRITE(OUT_LU,*) 'Press <return> for more'
          READ(IN_LU,'(A)') DUMMY
          J=0
        ENDIF
       ENDDO
      ELSE
       FOUND=.FALSE.
       DO WHILE (HELPSTR.NE.'HELPEND')
        READ(IO_LU,'(A)') HELPSTR
        IF (HELPSTR(1:8).EQ.'Command:') THEN
          SP=INDEX(HELPSTR,':')
          HELPSTR=HELPSTR((SP+2):)
          HELPSTR2=HELPSTR
          SP=INDEX(HELPSTR,' ')
          HELPSTR=HELPSTR(1:(SP-1))
          CALL TOLOWERC(HELPSTR)
          IF (HELPSTR(1:(SP-1)).EQ.CPARAM(1:(SP-1)) ) THEN
           WRITE(OUT_LU,'(1X,A)') HELPSTR2
           FOUND=.TRUE.
           DO WHILE (HELPSTR.NE.' ')
            READ(IO_LU,'(A)') HELPSTR
            IF (HELPSTR.NE.' ') THEN
             WRITE(OUT_LU,'(1X,A)') HELPSTR
            ENDIF
           ENDDO
          ENDIF 
        ENDIF
       ENDDO
       CLOSE(IO_LU)
        IF (.NOT.FOUND) THEN
         CALL WR_ERROR('Command not in help file',OUT_LU)
        ENDIF
       ENDIF

      GOTO 999
C
666   CONTINUE
      CALL WR_ERROR('Cannot open help file',OUT_LU)

999   CONTINUE
      CLOSE(IO_LU)
      END
