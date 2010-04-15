      SUBROUTINE GRCHSZ (IDENT,XSIZE,YSIZE,XSPACE,YSPACE)
*+
*
*     - - - - - - - -
*       G R C H S Z     (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Returns the default character attributes for the current device
*
*   Given
*      IDENT      i       Device identifier (IGNORED)
*
*   Returned
*      XSIZE      r       Default character width
*      YSIZE      r          "        "     height
*      XSPACE     r       Default horizontal spacing
*      YSPACE     r          "    vertical      "
*
*   Read from COMMON
*      GRCSCL     r()     Device character scale factor
*      GRCIDE     i       Current device identifier
*
*   Constants from GRECOM
*      GRCXSZ     r       Default character width
*      GRCYSZ     r          "        "     height
*      GRCXSP     r          "        "     horizontal spacing
*      GRCYSP     r          "        "     vertical      "
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      INTEGER IDENT
      REAL XSIZE, YSIZE, XSPACE, YSPACE

      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRCHSZ - No PGPLOT device open',
     : GRNODO)
      ELSE
         XSIZE = GRCXSZ * GRCSCL(GRCIDE)
         YSIZE = GRCYSZ * GRCSCL(GRCIDE)
         XSPACE = GRCXSP * GRCSCL(GRCIDE)
         YSPACE = GRCYSP * GRCSCL(GRCIDE)
      END IF

      END
