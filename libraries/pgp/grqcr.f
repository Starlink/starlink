      SUBROUTINE GRQCR(I, R, G, B)
*+
*     - - - - - - -
*       G R Q C R   (GKS emulation of GRPCKG)
*     - - - - - - -
*
*   Inquire the RGB colours of a colour index.
*
*   Given
*      I        i     Colour index
*
*   Returned
*      R        r     RGB colours
*      G        r
*      B        r
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*      GRWKID   i()   Workstation identifier
*
*   Constants from GKS_PAR
*      GREALI
*
*   D.L.Terrett  Starlink  Apr 1992
*+
      IMPLICIT NONE
      INTEGER I
      REAL R, G, B

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'

      INCLUDE 'GKS_PAR'

      INTEGER IERR

      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRQCR - No PGPLOT device open',
     :   GRNODO)
      ELSE

         CALL GQCR(GRWKID, I, GREALI, IERR, R, G, B)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRQCR', 'GQCR', IERR)
            GO TO 9999
         END IF
      END IF

 9999 CONTINUE
      END
