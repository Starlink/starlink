      SUBROUTINE GRSCI(ICOL)
*+
*     - - - - - - -
*       G R S C I    (GKS emulation of GRPCKG)
*     - - - - - - -
*
*   Set the current colour. If ICOL is less than zero the
*   colour is set to one. On devices that do not support colour
*   the line and marker colours are only allowed to be set to 0
*   or 1 to avoid drawing 'grey' (and therefore invisible) lines
*   on printers.
*
*   Given
*      ICOL     i     Colour index
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*
*   Written to COMMON
*      GRCCOL   i()   Current colour
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE

      INTEGER ICOL, IERR, NCOLI, COLA, NPCI

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRSCI - No PGPLOT device open',
     :   GRNODO)
      ELSE
         IF (ICOL.LT.0) THEN
            CALL MSG_SETI('COL', ICOL)
            CALL ERR_REP('GRILCO',
     :      'GRSCI - ^COL is not a valid colour; colour 1 selected',
     :      GRILCO)
            GRCCOL(GRCIDE) = 1
         ELSE
            GRCCOL(GRCIDE) = ICOL
         END IF

*     Inquire colour facilites
         CALL GQCF(GRTYP(GRCIDE), IERR, NCOLI, COLA, NPCI)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRSETCOL', 'GQCF', IERR)
         ELSE

*        Flush buffers and set fill area colour index
            CALL GRTERM
            CALL GSFACI(GRCCOL(GRCIDE))

            IF (COLA.EQ.1) THEN

*           Colour is supported
               CALL GSPLCI(GRCCOL(GRCIDE))
               CALL GSPMCI(GRCCOL(GRCIDE))
            ELSE

*           Workstation is monochrome
               IF (GRCCOL(GRCIDE).GE.1) THEN
                  CALL GSPLCI(1)
                  CALL GSPMCI(1)
               ELSE
                  CALL GSPLCI(0)
                  CALL GSPMCI(0)
               END IF
            END IF
         END IF

      END IF

      END
