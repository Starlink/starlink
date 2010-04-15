C+
      SUBROUTINE FIG_CSLICE (DATA,NX,NY,NELM,ENDS,IORD,SPECT)
C
C     F I G _ C S L I C E
C
C     Creates a 1D array by taking a slice through a two dimensional
C     array.  The slicing is done by using Everett interpolation, and
C     this routine is essentially a direct lift from the AIPS routine
C     SLICE.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) DATA    (Real array DATA(NX,NY)) The 2D array.
C     (>) NX      (Integer) The first dimension of DATA.
C     (>) NY      (Integer) The second dimension of DATA.
C     (>) NELM    (Integer) The number of elements in the 1D array.
C     (>) ENDS    (Real array ENDS(4)) The ends of the slice.  Elements
C                 1..4 are 1) & 2) X and Y values, respectively, of the
C                 start of the slice, 3) & 4) X and Y values of the end
C                 of the slice.  The values may be fractional, and for
C                 this purpose the CENTER of the first pixel is taken
C                 to be (1.0,1.0).
C     (>) IORD    (Integer) The order to be used for interpolation.
C                 This can be 0 => linear, 1 => cubic, 2 => quintic,
C                 3 => septic (sic).
C     (>) SPECT   (Real array SPECT(NELM)) The resulting 1D array.
C
C     Common variables used - None
C
C                                        KS / CIT 22nd March 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,NELM,IORD
      REAL DATA(NX,NY),ENDS(4),SPECT(NELM)
C
C     Local variables
C
      INTEGER I
      REAL DELTAX, DELTAY, DX, DY, XBLC, XTRC, YBLC, YTRC
C
C     Get the end values
C
      XBLC=ENDS(1)
      YBLC=ENDS(2)
      XTRC=ENDS(3)
      YTRC=ENDS(4)
C
C     Initialise the interpolation routines
C
      CALL INITEI(IORD)
C
C     Work through the elements of the spectrum
C
      DELTAX=(XTRC-XBLC)/FLOAT(NELM-1)
      DELTAY=(YTRC-YBLC)/FLOAT(NELM-1)
      DX=XBLC
      DY=YBLC
      DO I=1,NELM
         CALL FIG_ETERP(DX,DY,NX,NY,DATA,SPECT(I))
         DX=DX+DELTAX
         DY=DY+DELTAY
      END DO
C
      END
