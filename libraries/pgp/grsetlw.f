      SUBROUTINE GRSLW(IW)
*+
*     - - - - - - -
*       G R S L W     (GKS emulation of GRPCKG)
*     - - - - - - -
*
*   Set the current line width. If IW is less than one or greater than
*   twenty one the line width is set to one.
*
*   Given
*      IW      i     Line width
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*      GRTYP    i     Current device type
*
*   Written to COMMON
*      GRWIDT   i()   Current line width
*
*   D.L.Terrett  Starlink  Aug 1997
*+
      IMPLICIT NONE

      INTEGER IW, IERR, NLT, LT, NLW, NPPLI, DCUNIT, LX, LY
      REAL NOMLW, RLWMIN, RLWMAX, RX, RY, W

      INCLUDE 'grecom.inc'
      INCLUDE 'GKS_PAR'
      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRSLW - No PGPLOT device open',
     :   GRNODO)
      ELSE
         IF (IW.LE.0.OR.IW.GT.201) THEN
            CALL MSG_SETI('IW', IW)
            CALL ERR_REP('GRILLW',
     :    'GRSLW - ^IW is not a valid line width; width 1 selected',
     :      GRILLW)
            GRWIDT(GRCIDE) = 1
         ELSE

            GRWIDT(GRCIDE) = IW

*   If the device supports an infinite range of line widths then we assume
*   that it produces high quality thick lines with rounded ends properly
*   clipped.
            CALL GQPLF( GRTYP(GRCIDE), 1, IERR, NLT, LT, NLW, NOMLW,
     :                  RLWMIN, RLWMAX, NPPLI)
            IF (IERR.NE.0) THEN
               CALL GRQREP('GRSLW', 'GQPLF', IERR)
            ELSE IF (NLW.EQ.0) THEN
               CALL GRFLU0
               GRWIDT(GRCIDE) = -IW

*   Set the line width to 0.005 inch * IW
               CALL GQDSP( GRTYP(GRCIDE), IERR, DCUNIT, RX, RY, LX, LY)
               IF (IERR.NE.0) THEN
                  CALL GRQREP('GRSLW', 'GQDSP', IERR)
               ELSE
                  IF (DCUNIT.EQ.GMETRE) THEN
                     W = REAL(IW) * 0.005 * 0.0254
                  ELSE
                     W = REAL(IW) * 0.005 * GRXPIN(GRCIDE)
                  ENDIF
                  CALL GSLWSC(W / NOMLW)
               ENDIF
            END IF
         END IF
      END IF
      END
