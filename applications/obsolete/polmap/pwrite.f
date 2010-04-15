      SUBROUTINE PWRITE(FILENAME,TITLE,LAMBDA,STOKES_I,
     &                  STOKES_Q,STOKES_QV,
     &                  STOKES_U,STOKES_UV,NPTS,IO_LU,OUT_LU)
C+
C
C Subroutine:
C
C     P W R I T E
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C FILENAME (<), TITLE (<), LAMBDA (<), STOKES_I (<),
C STOKES_Q (<), STOKES_QV (<),
C STOKES_U (<), STOKES_UV (<), NPTS (<), IO_LU (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C
C This subroutine writes out a polarization spectrum from an ascii format
C file:
C Lambda,i,%p,%p*i
C
C
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      INTEGER IO_LU
      INCLUDE 'array_size.inc'
      INTEGER NPTS
      REAL POL
      REAL LAMBDA(MAXPTS)
      REAL STOKES_I(MAXPTS)
      REAL STOKES_Q(MAXPTS)
      REAL STOKES_QV(MAXPTS)
      REAL STOKES_U(MAXPTS)
      REAL STOKES_UV(MAXPTS)
      INTEGER I
      CHARACTER*(*) FILENAME,TITLE
C
      OPEN(UNIT = IO_LU,FILE = FILENAME,STATUS = 'UNKNOWN',
     &FORM = 'FORMATTED',ERR = 666)
C
10    FORMAT(1X,1P,E11.4,1X,E11.4,1X,E11.4,1X,E11.4,0P)
      DO I = 1,NPTS
       IF (STOKES_I(I).NE.0.) THEN
       POL=100.*SQRT(STOKES_Q(I)**2+STOKES_U(I)**2)/STOKES_I(I)
       WRITE(IO_LU,10,ERR = 667) LAMBDA(I),STOKES_I(I),POL,
     &                           POL*STOKES_I(I)/100.
       ENDIF
      ENDDO
      GOTO 999

666   CONTINUE
      CALL WR_ERROR('Cannot open file',OUT_LU)
      GOTO 999

667   CONTINUE
      CALL WR_ERROR('Error writing to file',OUT_LU)
      GOTO 999

999   CONTINUE
      CLOSE(IO_LU)
      END
