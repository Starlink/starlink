      SUBROUTINE WRALAS(FILENAME,TITLE,LAMBDA,STOKES_I,
     &                  STOKES_Q,STOKES_QV,
     &                  STOKES_U,STOKES_UV,NPTS,IO_LU,OUT_LU)
C+
C
C Subroutine: 
C
C     W R A L A S
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
C Lambda,i,q,qv,u,uv
C
C
C
C-
C
      IMPLICIT NONE
      INTEGER OUT_LU
      INTEGER IO_LU
      INCLUDE 'array_size.inc'
      INTEGER NPTS
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
10    FORMAT(1X,F8.2,1P,5E14.5,0P)
      DO I = 1,NPTS
      WRITE(IO_LU,10,ERR = 667) LAMBDA(I),STOKES_I(I),
     &STOKES_Q(I),STOKES_QV(I),STOKES_U(I),STOKES_UV(I)
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
