
C===========================================================================

      SUBROUTINE PERIOD_MODSINECURVE(IPARRAY, NUMROWS, NUMCOLS,
     :                               PERIOD, AMPLITUDE, ZEROPT,
     :                               GAMMA, OPTION, OPARRAY)

C===========================================================================
C Produces sine-curve data forrSINE. Both arrays can correspond to
C slices of dynamically-allocated memory, provided that the appropriate
C "calling" arguments are memory pointers being passed via the %VAL()
C intrinsic function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INCLUDE "PIVARS"

      INTEGER NUMROWS, NUMCOLS, I
      DOUBLE PRECISION IPARRAY(NUMROWS,NUMCOLS)
      DOUBLE PRECISION OPARRAY(NUMROWS,NUMCOLS)
      DOUBLE PRECISION PERIOD, AMPLITUDE, ZEROPT, GAMMA
      CHARACTER*1 OPTION


C-----------------------------------------------------------------------------
C Add, subtract, multiply or divide sine curve.
C-----------------------------------------------------------------------------

      DO 10 I = 1, NUMROWS
         OPARRAY(I, 1) = IPARRAY(I, 1)
         OPARRAY(I, 3) = IPARRAY(I, 3)
         IF ( OPTION.EQ.'A' ) THEN
            OPARRAY(I, 2) = IPARRAY(I, 2)
     :                         + (GAMMA+(AMPLITUDE*DSIN(((TWOPI)
     :                         /PERIOD)*(IPARRAY(I, 1)-ZEROPT))))
         ELSE IF ( OPTION.EQ.'M' ) THEN
            OPARRAY(I, 2) = IPARRAY(I, 2)
     :                         *(GAMMA+(AMPLITUDE*DSIN(((TWOPI)
     :                         /PERIOD)*(IPARRAY(I, 1)-ZEROPT))))
         ELSE IF ( OPTION.EQ.'D' ) THEN
            OPARRAY(I, 2) = IPARRAY(I, 2)
     :                         /(GAMMA+(AMPLITUDE*DSIN(((TWOPI)
     :                         /PERIOD)*(IPARRAY(I, 1)-ZEROPT))))
         ELSE IF ( OPTION.EQ.'S' .OR. OPTION.EQ.' ' ) THEN
            OPARRAY(I, 2) = IPARRAY(I, 2)
     :                         - (GAMMA+(AMPLITUDE*DSIN(((TWOPI)
     :                         /PERIOD)*(IPARRAY(I, 1)-ZEROPT))))
         END IF
   10 CONTINUE

      RETURN
      END
