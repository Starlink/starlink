      SUBROUTINE READ_STK(TOP_STK,STK_NPTS,STK_LAMBDA,STK_STOKES_I,
     &STK_STOKES_Q,
     &STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,STK_TITLE,NO_IN_SAVE,
     &SPEC_SIZE,NPTS,WA,IAR,
     &Q,QV,U,UV,TMP_STK_TITLE,OUT_LU)
C+
C
C Subroutine: 
C
C
C   R E A D _ S T K
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C TOP_STK (>), STK_NPTS (>), STK_LAMBDA (>), STK_STOKES_I (>),
C STK_STOKES_Q (>), STK_STOKES_QV (>), STK_STOKES_U (>), STK_STOKES_UV (>),
C STK_TITLE (>), NO_IN_SAVE (<), SPEC_SIZE (<), NPTS (<), WA (<), IAR (<),
C Q (<), QV (<), U (<), UV (<), TMP_STK_TITLE (<), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C Reads in the stack from the mapped arrays
C
C
C
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      INCLUDE 'array_size.inc'
C
C The actual stack arrays
C
      INTEGER TOP_STK
      REAL STK_STOKES_I(MAXPTS,MAXSPEC)
      REAL STK_STOKES_Q(MAXPTS,MAXSPEC)
      REAL STK_STOKES_QV(MAXPTS,MAXSPEC)
      REAL STK_STOKES_U(MAXPTS,MAXSPEC)
      REAL STK_STOKES_UV(MAXPTS,MAXSPEC)
      REAL STK_LAMBDA(MAXPTS,MAXSPEC)
      INTEGER STK_NPTS(MAXSPEC)
      CHARACTER*(*) STK_TITLE(*)
C
C The saved stack arrays that have been mapped
C 
      INTEGER NO_IN_SAVE
      INTEGER MAXNO,SPEC_SIZE
      REAL IAR(SPEC_SIZE,NO_IN_SAVE)
      REAL Q(SPEC_SIZE,NO_IN_SAVE)
      REAL QV(SPEC_SIZE,NO_IN_SAVE)
      REAL U(SPEC_SIZE,NO_IN_SAVE)
      REAL UV(SPEC_SIZE,NO_IN_SAVE)
      INTEGER NPTS(NO_IN_SAVE)
      REAL WA(SPEC_SIZE,NO_IN_SAVE)
      CHARACTER*(*) TMP_STK_TITLE(*)
C
C Misc.
C
      INTEGER I,J
C
C
      MAXNO = NO_IN_SAVE
      IF ((MAXNO+TOP_STK).GT.MAXSPEC) THEN
        CALL WR_ERROR('Saveset too large for stack',OUT_LU)
        CALL WR_ERROR('Cannot read in all spectra',OUT_LU)
        MAXNO = MAXSPEC-TOP_STK
      ENDIF
C
C Read in the stack
C                   
      DO J = 1,MAXNO
       DO I = 1,SPEC_SIZE
        STK_STOKES_I(I,TOP_STK+J) = IAR(I,J)
        STK_LAMBDA(I,TOP_STK+J) = WA(I,J)
        STK_STOKES_Q(I,TOP_STK+J) = Q(I,J)
        STK_STOKES_QV(I,TOP_STK+J) = QV(I,J)
        STK_STOKES_U(I,TOP_STK+J) = U(I,J)
        STK_STOKES_UV(I,TOP_STK+J) = UV(I,J)
       ENDDO
       STK_TITLE(TOP_STK+J) = TMP_STK_TITLE(J)
       STK_NPTS(TOP_STK+J) = NPTS(J)
      ENDDO
      TOP_STK = TOP_STK+MAXNO
      END

