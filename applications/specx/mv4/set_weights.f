*  History:
*     28 Jan 1994 (hme):
*        Remove the desparate TYPE* statements.
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*      4 Feb 2002 (ajc):
*        Replace non-standard PRINT statement using DO loop
*-----------------------------------------------------------------------

      SUBROUTINE SET_INTERP_WEIGHTS (FWHM, WMAX, IFAIL)

      IMPLICIT   NONE

*     Formal parameters

      REAL       FWHM
      REAL       WMAX
      INTEGER    IFAIL

*     Include files

      INCLUDE   'MAPHD'
      INCLUDE   'WEIGHTS'

C     Local variables

      INTEGER   I, J
      REAL      XRAD, YRAD   ! 1/e interpolating function width (pixels)

C   Ok, go...

C     Zero weights array

      DO I = 0, IWMAX
        DO J = 0, IWMAX
          WEIGHT(I,J) = 0.0
        END DO
      END DO

C     Check for pathology

      IXMAX = 0
      IYMAX = 0
      WEIGHT(0,0) = 1.0

      IF (FWHM.EQ.0.0 .OR. WMAX.EQ.0.0) RETURN

C     Interpolating function supplied through FLAGCOMM by ask_interp
C     Normalize interpolating function to pixels

      XRAD  = 0.5*FWHM/CELL_XSIZE/SQRT(ALOG(2.))
      IXMAX = INT (WMAX/CELL_XSIZE)
      YRAD  = 0.5*FWHM/CELL_YSIZE/SQRT(ALOG(2.))
      IYMAX = INT (WMAX/CELL_YSIZE)

      IF (IXMAX.GT.IWMAX .OR. IYMAX.GT.IWMAX) THEN
        IFAIL = 47  ! Maximum radius exceeded.
        RETURN
      END IF

C     Set up the weights array (to save working out lots of exponentials later

      DO I = 0,IXMAX
        DO J = 0,IYMAX
          WEIGHT(I,J) = EXP (-((I**2/XRAD**2)+(J**2/YRAD**2)))
        END DO
      END DO

      IF (IXMAX.LT.10 .AND. IYMAX.LT.10) THEN
        PRINT *
        PRINT *, 'Weights array:'
        DO J = 0, IYMAX
           PRINT '(10(2X,F9.5))',(WEIGHT(I,J),I=0,IXMAX)
        END DO
      ELSE
        PRINT *
        PRINT *, 'Weights array too big to type!'
      END IF

      RETURN
      END

*-----------------------------------------------------------------------
