      SUBROUTINE GRSFNT(IFONT)
*+
*     - - - - - - - -
*       G R S F N T     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Set the current font. If IFONT is less than one or greater than four
*   the font is set to one.
*
*   Given
*      IFONT    i     Font number
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*
*   Written to COMMON
*      GRCFNT   i()   Current font
*
*   D.L.Terrett  Starlink  Jul 1987
*+
      IMPLICIT NONE

      INTEGER IFONT

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRSETCOL - No PGPLOT device open',
     :   GRNODO)
      ELSE
         IF (IFONT.LE.0.OR.IFONT.GT.4) THEN
            CALL MSG_SETI('FONT', IFONT)
            CALL ERR_REP('GRILFO',
     :      'GRSETCOL - ^FONT is not a valid font; font 1 selected',
     :      GRILFO)
            GRCFNT(GRCIDE) = 1
         ELSE
            GRCFNT(GRCIDE) = IFONT
         END IF
      END IF

      END
