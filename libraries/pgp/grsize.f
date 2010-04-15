      SUBROUTINE GRSIZE (IDENT,XSZDEF,YSZDEF,XSZMAX,YSZMAX,
     :                                            XPERIN,YPERIN)
*+
*     - - - - - - - -
*       G R S I Z E    (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Returns size (in raster units) and resolution of device (in pixels
*   per inch.
*
*   Given
*      IDENT    i     Device identifier (IGNORED)
*
*   Returned
*      XSZDEF   r     Default size of device in X
*      YSZDEF   r     Default size of device in Y
*      XSZMAX   r     Maximum size of device in X
*      YSZMAX   r     Maximum size of device in Y
*      XPERIN   r     X resolution (pixels/inch)
*      YPERIN   r     Y resolution (pixels/inch)
*
*   Read from COMMON
*      GRCIDE   i     Current device
*      GRXMAX   r()   Device size (x)
*      GRYMAX   r()   Device size (y)
*      GRXMAX   r()   Maximum extent (x)
*      GRYMAX   r()   Maximum extent (y)
*      GRXPIN   r()   Resolution (x)
*      GRYPIN   r()   Resolution (y)
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'

      INCLUDE 'GKS_PAR'


      INTEGER IDENT
      REAL XSZDEF,YSZDEF,XSZMAX,YSZMAX,XPERIN,YPERIN

      REAL XM,YM
      INTEGER IERR, LX, LY, IUNIT

      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRSIZE - No PGPLOT device open',
     :   GRNODO)
      ELSE

*      If GRPCKG opened the workstation then return the true size
         IF (GRWSOP(GRCIDE)) THEN

*         Find the size and resolution of the workstation
            CALL GQDSP(GRTYP(GRCIDE),IERR,IUNIT,XM,YM,LX,LY)
            IF (IERR.NE.0) THEN
               CALL GRQREP('GRSIZE', 'GQMDS', IERR)
               GO TO 9999
            END IF

            XSZMAX = REAL(LX-1)
            YSZMAX = REAL(LY-1)

         ELSE

*      Return virtual size as recorded in common
            XSZMAX = GRXMAX(GRCIDE) - GRXMIN(GRCIDE)
            YSZMAX = GRYMAX(GRCIDE) - GRYMIN(GRCIDE)
         END IF

*     Default size is always maximum size
         XSZDEF = XSZMAX
         YSZDEF = YSZMAX

*  Resolution
         XPERIN = GRXPIN(GRCIDE)
         YPERIN = GRYPIN(GRCIDE)

      END IF
 9999 CONTINUE
      END
