      SUBROUTINE GRPIXL(IA,IDIM,JDIM,I1,I2,J1,J2,X1,X2,Y1,Y2)
*+
*
*     - - - - - - - -
*       G R P I X L     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Plots a cell array
*
*   Given
*      IA       i()   Data array
*      IDIM     i     First dimension of A
*      JDIM     i     Second dimension of A
*      I1       i     Array subset
*      I2       i       "     "
*      J1       i       "     "
*      J2       i       "     "
*      X1       r     Coordinate of array element I1,J1
*      Y1       r          "      "   "      "      "
*      X2       r     Coordinate of array element I2,J2
*      Y2       r          "      "   "      "      "
*
*   Read from COMMON
*      GRCIDE   i     Current device
*      GRXORG   r()   x origin
*      GRYORG   r()   y origin
*      GRXSCL   r()   x scale
*      GRYSCL   r()   y scale
*
*   D.L.Terrett  Starlink  Apr 1991
*   D.L.Terrett  Starlink  Feb 1995
*      Pass rows/cols not end row/col to GCA
*+
      IMPLICIT NONE

      INTEGER IDIM, JDIM, I1, I2, J1, J2, IA(IDIM,JDIM)
      REAL X1, X2, Y1, Y2

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      REAL XT1, XT2, YT1, YT2

      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRPIXL - No PGPLOT device open',
     :   GRNODO)
      ELSE

         XT1 = X1 * GRXSCL(GRCIDE) + GRXORG(GRCIDE)
         YT1 = Y1 * GRYSCL(GRCIDE) + GRYORG(GRCIDE)
         XT2 = X2 * GRXSCL(GRCIDE) + GRXORG(GRCIDE)
         YT2 = Y2 * GRYSCL(GRCIDE) + GRYORG(GRCIDE)
         CALL GCA(XT1,YT1,XT2,YT2,IDIM,JDIM,I1,J1,(I2-I1+1),(J2-J1+1),
     :            IA)
      ENDIF
      END
