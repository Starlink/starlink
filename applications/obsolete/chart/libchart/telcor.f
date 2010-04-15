      SUBROUTINE TELCOR(AJ,DJ,AJE,DJE,AJW,DJW, STATUS )
*+
*   Subroutine to correct field centre positions for the
*   errors in alignment of the 26" telescope polar axis.
*
*   Adapted from the ICL1903T version of Chart
*   by K F Hartley at RGO on 1-2-83
*
*   Arguments:
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     4-MAR-1993 (AJJB):
*       STATUS argument added.
*     22-MAR-1993 (AJJB):
*        Commented out declarations of local variables which are never
*        used.
*
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      DOUBLE PRECISION AJ,DJ,AJE,DJE,AJW,DJW
      DOUBLE PRECISION TWOPI,HALFPI,RDSA,RDST,RDDG
      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG
      DOUBLE PRECISION A3,C0,C1,C2,C3,HALFM
*     DOUBLE PRECISION BO, A1, B1, A2, B2, B3
      DOUBLE PRECISION DK,S,DDEC
      DOUBLE PRECISION G,R

      DATA A3/0.40724D-3/
      DATA C0,C1,C2,C3/-1.739,-0.4808,0.2362D-4,0.2200D-4/
      DATA HALFM/0.145444D-3/

*
*   Define a double precision function.
*
      G(R)=(C0+R*(C1+R*(C2+R*C3)))*RDSA
*
*   Now get on with the calculation
*

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DK=DJ/RDDG
      S=COS(DJ)
      IF (S.EQ.0.0D1) S=1.0D-3
      DDEC= -(36.0+1.62*DK)*RDSA
      DJW=DJ+DDEC
      AJW=AJ+(8.15*DK-255.0)*RDSA/S
      DJE=DJ+A3
      AJE=AJ+15.0*(G(DK)/S-47.0*RDSA)
*
*   Round to the nearest half minute of arc
*
      IF (DJW.NE.0.0D0) DJW=DJW+SIGN(HALFM,DJW)
      IF (DJE.NE.0.0D0) DJE=DJE+SIGN(HALFM,DJE)
      END
