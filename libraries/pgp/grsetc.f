      SUBROUTINE GRSETC (ID,XSIZE)
*+
*     - - - - - - - -
*       G R S E T C     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Set the character size. If XSIZE is less than or equal to zero the
*   size is set to the default
*
*   Given
*      ID       i     Device identifier (IGNORED)
*      XSIZE    r     Character scale factor
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*
*   Written to COMMON
*      GRCFAC   r()   Character scale factor
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE

      INTEGER ID
      REAL XSIZE

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRSETC - No PGPLOT device open',
     :   GRNODO)
      ELSE
         IF (XSIZE.LE.0.0) THEN
            GRCFAC(ID) = 1.0
         ELSE
            GRCFAC(ID) = XSIZE / GRCXSZ
         END IF
      END IF

      END
