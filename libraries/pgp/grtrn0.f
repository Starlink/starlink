      SUBROUTINE GRTRN0(XORG,YORG,XSCALE,YSCALE)
*+
*     - - - - - - - -
*       G R T R N 0    (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Sets the scaling from absolute to scaled coordinates
*
*   Given
*      XORG     r     Absolute X coordinate of origin
*      YORG     r         "    Y     "       "   "
*      XSCALE   r     X scale (absolute/scaled)
*      YSCALE   r     Y   "         "     "
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*
*   Written to COMMON
*      GRXSCL   r()   X scale
*      GRYSCL   r()   Y scale
*      GRXORG   r()   X origin
*      GRYORG   r()   Y origin
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE

      REAL XORG, YORG, XSCALE, YSCALE

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRTRN0 - No PGPLOT device open',
     :   GRNODO)
      ELSE
         GRXSCL(GRCIDE) = XSCALE
         GRYSCL(GRCIDE) = YSCALE
         GRXORG(GRCIDE) = XORG
         GRYORG(GRCIDE) = YORG
      END IF

      END
