      SUBROUTINE GRCLOS
*+
*
*     - - - - - - - -
*       G R C L O S     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Update and then close the currently selected device.
*
*   Read from COMMON
*      GRCIDE   i     Current GRPCKG device id
*      GRDVOP   l()   Device open flag
*      GRWKID   i()   Workstation id
*      GRWSOP   l()   W/S opened by GRPCKG
*
*   Written to  COMMON
*      GRCIDE   i     Current GRPCKG device id
*      GRDVOP   l()   Device open flag
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRCLOS - No PGPLOT device open',
     :                 GRNODO)
      ELSE
         CALL GRTERM

*     If GRPCKG opened the workstation then close it otherwise restore
*     the global GKS attributes that GRPCKG may have changed.
         IF (GRWSOP(GRCIDE)) THEN
            CALL GRCLWK(GRWKID(GRCIDE))
         ELSE
            CALL GRSAVE(.FALSE.)
         END IF

*     Mark the workstation as closed and invalidate the workstation index
         GRDVOP(GRCIDE) = .FALSE.
         GRCIDE = 0
      END IF

      END
