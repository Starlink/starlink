      SUBROUTINE GET_LIMITS(XARRAY,N1,N2,XMAX,XMIN)
C+
C
C Subroutine:
C
C    G E T _ L I M I T S
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C XARRAY (<), N1 (<), N2 (<), XMAX (>), XMIN (>)
C
C History:
C
C   May 1994 Created
C
C
C
C
C Given an array this routine finds the limits (used for plotting) which
C give a border of 5% of the data range above and below the maximum and
C minimum data values.
C
C-

      IMPLICIT NONE
      REAL XARRAY(*),XMAX,XMIN,DX
      INTEGER N1,N2,I
      XMAX=-1.E30
      XMIN=1.E30
      DO I=N1,N2
        XMAX=MAX(XMAX,XARRAY(I))
        XMIN=MIN(XMIN,XARRAY(I))
      ENDDO
      DX=XMAX-XMIN
      IF (DX .LT. 1.E-30) THEN
        DX=XMAX
      ENDIF
      XMAX = XMAX+0.05*DX
      XMIN = XMIN-0.05*DX
      IF (XMAX.EQ.XMIN) THEN
       XMAX=XMAX+1.E-5
      ENDIF
      END

