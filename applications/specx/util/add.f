*  History:
*     19 Nov 1993 (hme):
*        Remove TABs.
C-----------------------------------------------------------------------

      SUBROUTINE ADD (NQ, IFAIL)

C   Routine to add spectra in X and Y stack positions

      IMPLICIT  NONE

      INTEGER   NQ
      INTEGER   IFAIL

      INCLUDE  'FLAGCOMM'
      INCLUDE  'STACKCOMM'
      INCLUDE  'SPECX_PARS'
      INCLUDE  'STAKPAR'

      REAL*4      STACK(1)
      EQUIVALENCE (STACK(1),SCAN_HEADER(1))

      INTEGER   I

*  Ok, go...

      DO I = 1, LSTK-LHEAD
        IF (DATA(I).NE.BADPIX_VAL) DATA(I) = -DATA(I)
      END DO

      CALL SU (NQ, IFAIL)

      RETURN
      END
