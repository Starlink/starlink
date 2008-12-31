      SUBROUTINE RDALAS(FILENAME,TITLE,LAMBDA,STOKES_I,
     &                  STOKES_Q,STOKES_QV,
     &                  STOKES_U,STOKES_UV,NPTS,IO_LU,OUT_LU)
C+
C
C Subroutine: 
C
C    R D A L A S
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C FILENAME (<), TITLE (>), LAMBDA (>), STOKES_I (>),
C STOKES_Q (>), STOKES_QV (>),
C STOKES_U (>), STOKES_UV (>), NPTS (>), IO_LU (<), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C
C
C This subroutine reads in a polarization spectrum from an ascii format
C file. The file is 6 columns across:
C Wavelength,Stokes I,Stokes Q,variance,Stokes U,variance
C
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      INTEGER IO_LU
      INCLUDE 'array_size.inc'
C
C The current arrays...
C
      INTEGER NPTS
      REAL LAMBDA(*)
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
C
      CHARACTER*(*) FILENAME               ! The filename
      CHARACTER*(*) TITLE                  ! The title for the spectrum
C
      OPEN(UNIT = IO_LU,FILE = FILENAME,STATUS = 'OLD',
     &FORM = 'FORMATTED',ERR = 666)
C
      NPTS = 1
      DO WHILE(.TRUE.)
        READ(IO_LU,*,END = 30,ERR = 667) LAMBDA(NPTS),STOKES_I(NPTS),
     &  STOKES_Q(NPTS),STOKES_QV(NPTS),STOKES_U(NPTS),STOKES_UV(NPTS)
      NPTS = NPTS+1
      IF (NPTS.GT.MAXPTS) THEN
       CALL WR_ERROR('Spectrum too big to read in',OUT_LU)
       NPTS=0
       GOTO 666
      ENDIF
      END DO
30    CONTINUE
      NPTS = NPTS-1
      TITLE = FILENAME
      GOTO 999
C
C The crash out routines
C
666   CONTINUE
      CALL WR_ERROR('Cannot open file',OUT_LU)
      GOTO 999
C
667   CONTINUE
      CALL WR_ERROR('Error reading from file',OUT_LU)
      NPTS=0
      GOTO 999

999   CONTINUE
      CLOSE(IO_LU)
      END
