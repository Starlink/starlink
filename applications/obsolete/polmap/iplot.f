      SUBROUTINE IPLOT(TITLE,LAMBDA,STOKES_I,
     &                  NPTS,BOX,
     &                  IAUTO,IMAX,IMIN,WAUTO,WMAX,WMIN,OUT_LU)
C+
C
C Subroutine: 
C
C    I P L O T
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C TITLE (<), LAMBDA (<), STOKES_I (<), NPTS (<), BOX (<),
C IAUTO (<), IMAX (<), IMIN (<), WAUTO (<), WMAX (<), WMIN (<), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C
C Plots the intensity array of the current polarization spectrum
C
C 
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      INCLUDE 'array_size.inc'
C
C The current arrays
C
      REAL STOKES_I(*)
C
      REAL LAMBDA(*)
      CHARACTER*(80) TITLE
      INTEGER NPTS
C
C The plotting ranges and flags
C
      REAL IMAX,IMIN
      LOGICAL IAUTO
      REAL WMAX,WMIN
      LOGICAL WAUTO
      LOGICAL BOX
C
C Misc.
C
      INTEGER I
      INTEGER WS,WE
C
C Set the plotting ranges
C
      IF (WAUTO.AND.BOX) THEN 
       WMAX=LAMBDA(NPTS)
       WMIN=LAMBDA(1)
      ENDIF
C
      CALL LOCATE(LAMBDA,NPTS,WMIN,WS)
      CALL LOCATE(LAMBDA,NPTS,WMAX,WE)

C
      IF (IAUTO.AND.BOX) THEN
       IMIN = 1.E30
       IMAX = -1.E30
       DO I = WS,WE
        IMIN = MIN(IMIN,STOKES_I(I))
       IMAX = MAX(IMAX,STOKES_I(I))
       ENDDO
       IMIN = IMIN*0.95
       IMAX = IMAX*1.05
      ENDIF
C
C Only clear the plotting window when box is set
C
      IF (BOX) THEN
       CALL PGENV(WMIN,WMAX,IMIN,IMAX,0,0)
       CALL PGLABEL('Wavelength','Stokes I',TITLE)
      ENDIF
C
C Plot the spectrum
C
      CALL PGBIN(NPTS,LAMBDA,STOKES_I,.TRUE.)
      END
