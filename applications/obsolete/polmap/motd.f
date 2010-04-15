      SUBROUTINE MOTD(IO_LU,TRANS,OUT_LU)
C+
C
C Subroutine:
C
C   M O T D
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C IO_LU (<), TRANS (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C Displays the current "message of the day" which is located in
C POLMAP_DIR/polmap.mtd
C
C
C-

C
      IMPLICIT NONE
      INTEGER IO_LU,OUT_LU
      INTEGER I
      CHARACTER*79 MOTDSTR
      CHARACTER*80 FILENAME
      CHARACTER*(*) TRANS
C
      I=INDEX(TRANS,' ')
      FILENAME=TRANS(1:(I-1))//'/polmap.mtd'
      OPEN(UNIT=IO_LU,FILE=FILENAME,
     &     STATUS='OLD',ERR=666)
      DO WHILE(.TRUE.)
       READ(IO_LU,'(A)',END=30) MOTDSTR
       WRITE(OUT_LU,'(1X,A)') MOTDSTR
      ENDDO
30    CONTINUE
      GOTO 999
666   CONTINUE
      CALL WR_ERROR('Cannot open message file',OUT_LU)
999   CONTINUE
      CLOSE(IO_LU)
      END
