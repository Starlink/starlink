      SUBROUTINE GRQCOL(ICLOW, ICHIGH)
*+
*     - - - - - - - -
*       G R Q C O L   (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Inquire the range of colour indices available on the current device.
*
*   If the background colour can be written then ICLOW is set to 0, if
*   it cannot ICLOW is set to 1. This cannot be determined reliably for
*   a GKS workstation so we guess that if a workstation can change its
*   colour table dynamically then it can change the background too.
*
*
*   Given
*      ICLOW    i     Lowest available colour index
*      ICHIGH   i     Highest available colour index
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*      GRWKID   i()   Workstation identifier
*      GRTYP    i()   Workstation type
*
*   Constants from GKS_PAR
*
*   D.L.Terrett  Starlink  Jun 1989
*+
      IMPLICIT NONE
      INTEGER ICLOW, ICHIGH

      INCLUDE 'grecom.inc'
      INCLUDE 'PGP_ERR'
      INCLUDE 'GKS_PAR'

      INTEGER IERR, MPLBTE, MPMBTE, MTXBTE, MFABTE, MPAI
      INTEGER PLBUN, PMBUN, TXBUN, FABUN, PAREP, COLREP, WKTR

      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRQCOL - No PGPLOT device open',
     :   GRNODO)
      ELSE

*     High limit is just the maximum size of the colour index table
*     minus one.
         CALL GQLWK(GRTYP(GRCIDE),IERR,MPLBTE,MPMBTE,MTXBTE,MFABTE,MPAI,
     1              ICHIGH)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRQCOL', 'GQLWK', IERR)
            GO TO 9999
         END IF
         ICHIGH = ICHIGH - 1

*     Low limit is 0 if the colour table is dynamic, otherwise it is 1.
         CALL GQDWKA(GRTYP(GRCIDE), IERR, PLBUN, PMBUN, TXBUN, FABUN,
     :               PAREP, COLREP, WKTR)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRQCOL', 'GQDWKA', IERR)
            GO TO 9999
         END IF

         IF (COLREP.EQ.GIMM) THEN
            ICLOW = 0
         ELSE
            ICLOW = 1
         ENDIF
      END IF

 9999 CONTINUE
      END
