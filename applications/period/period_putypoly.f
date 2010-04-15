
C===========================================================================

      SUBROUTINE PERIOD_PUTYPOLY(IPARRAY, NUMROWS, MXCOL, PFT,
     :                           MAXPOLY, NPOLY, OPARRAY)

C===========================================================================
C Outputs poly/rms data from DETREND. All arrays can correspond to slices
C of dynamically-allocated memory, provided that the appropriate "calling"
C arguments are memory pointers being passed via the %VAL() intrinsic
C function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, MXCOL, MAXPOLY, NPOLY, I
      DOUBLE PRECISION IPARRAY(NUMROWS,MXCOL), PFT(MAXPOLY)
      DOUBLE PRECISION OPARRAY(NUMROWS,MXCOL)
      DOUBLE PRECISION RMS, PERIOD_POLY


      RMS = 0.0D0
      DO 10 I = 1, NUMROWS
         OPARRAY(I, 1) = IPARRAY(I, 1)
         OPARRAY(I, 2) = IPARRAY(I, 2)
     :                   - PERIOD_POLY(PFT, NPOLY, IPARRAY(I, 1))
         OPARRAY(I, 3) = IPARRAY(I, 3)
         RMS = RMS + (OPARRAY(I, 2)**2)
  10  CONTINUE

      RMS = DSQRT(RMS/DFLOAT(NUMROWS))
      WRITE (*, *) ' '
      WRITE (*, *) '** OK: RMS of polynomial fit = ', RMS

      RETURN
      END
