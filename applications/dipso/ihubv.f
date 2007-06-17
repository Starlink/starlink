*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE IHUBV
*
*   CONVERTS U,B,V MAGS TO FLUXES
*
*   IMPORTS:
*     VARRAY   (REAL)  FIRST 3 ELEMENTS OF ARRAY CONTAIN U,B,V
*                      4TH ELEMENT IS HALF-WIDTH OF PLOT SYMBOL
*     ASIZE1   (INTEGER) MAXIMUM SIZE OF (WAVE,FLUX) ARRAYS
*     MAXBRK   (INTEGER) MAXIMUM SIZE OF 'BREAK' ARRAY
*
*   EXPORTS:
*     WAVE     (REAL) ARRAY OF WAVELENGTHS
*     FLUX     (REAL) ARRAY OF FLUXES
*     BREAK    (INTEGER) ARRAY OF 'BREAK' POINTS
*
*   ZERO MAGNITUDE ASSUMED TO MEAN NO DATA
*   NORMALISATION CONSTANTS:
*     V - MEAN OF HAYES & LATHAM, OKE & SCHILD, & TUG ET AL
*     U, B - FROM KURUCZ VEGA MODEL, NORMALISED TO V
*   EFFECTIVE WAVELENGTHS FROM A.Q.
*
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE IHUBV(VARRAY,ASIZE1,WAVE,FLUX,MAXBRK,BREAK,NPOINT,
     :                  NBREAK)
*
*   DECLARATIONS
*
       IMPLICIT NONE
*
       INTEGER ASIZE1, MAXBRK, NPOINT, NBREAK
       INTEGER BREAK(MAXBRK)
*
       REAL VARRAY(10)
       REAL WAVE(ASIZE1), FLUX(ASIZE1)
       REAL UMAG, BMAG, VMAG
       REAL XRNG
       REAL UCONST, BCONST, VCONST
       REAL UFLX, BFLX, VFLX
*
*   CONSTANT TERMS
*
       UCONST = -20.94
       BCONST = -20.51
       VCONST = -21.12
*
*
*
       UMAG = VARRAY(1)
       BMAG = VARRAY(2)
       VMAG = VARRAY(3)
       XRNG = VARRAY(4)
       IF (XRNG.GT.400.0) XRNG = 400.0
*
       NBREAK = 0
       NPOINT = 0
*
*
*
       IF (UMAG.NE.0.0) THEN
          UFLX = 10.0**(-((UMAG-UCONST)*0.4))
          WAVE(NPOINT+1) = 3600.0 - XRNG
          WAVE(NPOINT+2) = 3600.0
          WAVE(NPOINT+3) = 3600.0 + XRNG
          FLUX(NPOINT+1) = UFLX
          FLUX(NPOINT+2) = UFLX
          FLUX(NPOINT+3) = UFLX
          NPOINT = NPOINT + 3
          NBREAK = NBREAK + 1
          BREAK(NBREAK) = NPOINT
       ENDIF
*
       IF (BMAG.NE.0.0) THEN
          BFLX = 10.0**(-((BMAG-BCONST)*0.4))
          WAVE(NPOINT+1) = 4400.0 - XRNG
          WAVE(NPOINT+2) = 4400.0
          WAVE(NPOINT+3) = 4400.0 + XRNG
          FLUX(NPOINT+1) = BFLX
          FLUX(NPOINT+2) = BFLX
          FLUX(NPOINT+3) = BFLX
          NPOINT = NPOINT + 3
          NBREAK = NBREAK + 1
          BREAK(NBREAK) = NPOINT
       ENDIF
*
       IF (VMAG.NE.0.0) THEN
          VFLX = 10.0**(-((VMAG-VCONST)*0.4))
          WAVE(NPOINT+1) = 5500.0 - XRNG
          WAVE(NPOINT+2) = 5500.0
          WAVE(NPOINT+3) = 5500.0 + XRNG
          FLUX(NPOINT+1) = VFLX
          FLUX(NPOINT+2) = VFLX
          FLUX(NPOINT+3) = VFLX
          NPOINT = NPOINT + 3
          NBREAK = NBREAK + 1
          BREAK(NBREAK) = NPOINT
       ENDIF
*
       END
