      SUBROUTINE GRLINA (X,Y)
*+
*   - - - - - - - -
*     G R L I N A     (GKS emulation of GRPCKG)
*   - - - - - - - -
*
*   Draw line from current position to a specified position.
*
*   Given
*      X        r      New x position
*      Y        r      New y position
*
*   Read from COMMON
*      GRCIDE   i      Current device id
*      GRXSCL   r()    x scale factor
*      GRYSCL   r()    y scale factor
*      GRXORG   r()    x origin
*      GRYORG   r()    y origin
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      REAL     X,Y

      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRLINA - No PGPLOT device open',
     :   GRNODO)
      ELSE
         CALL GRLIN0( X * GRXSCL(GRCIDE) + GRXORG(GRCIDE),
     :                Y * GRYSCL(GRCIDE) + GRYORG(GRCIDE) )
      END IF

      END
