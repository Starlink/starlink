      SUBROUTINE OFFS(X,Y, STATUS )
*+
*   This subroutine simply prints the 'standard' offsets
*   in secs and arcsecs.
*
*   It is a variation on the ICL 1903T version.
*
*   Entered by K F Hartley at RGO on 2-2-83
*
*   Gets
*   ----
*      X,Y  - Input Position
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      DOUBLE PRECISION HALFPI,TWOPI,RDSA,RDST,RDDG,X,Y
      COMMON /CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG
*
*   Now do the Conversion.
*

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IXC=NNINT(X/RDDG*3600.0)
      IYC=NNINT(Y/RDDG*3600.0)
      WRITE (7,900) IXC,IYC
  900 FORMAT ('+',54X,2(I6,1X))
      END
