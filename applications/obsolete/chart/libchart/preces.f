      SUBROUTINE PRECES(AJ,DJ,AP,DP,EPI,EPF, STATUS )
*+
*   This Routine Precesses Input RA & Dec
*   from Epoch EPI to Epoch EPF
*
*   Gets
*   ----
*      AJ  - R.A. in Radians
*      DJ  - Dec. in Radians
*      EPI - Initial Epoch as a Date (Real)
*      EPF - Final Epoch      "    "      "
*
*   Returns
*   -------
*      AP  - The Precessed RA Returned
*      DP  -  "      "     Dec    "
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*   History:
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*-

      IMPLICIT DOUBLE PRECISION(A-Z)
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      REAL EPI,EPF
      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF (ABS(EPF-EPI).GT.1E-6) THEN
         T0 = (EPI-1900.0)/100.0
         T = (EPF-EPI)/100.0
         ZETA =((2304.250 + 1.396*T0)*T + 0.302*T*T + 0.018*T**3)
     :         * RDSA
         ZED = ZETA + (0.791*T*T * RDSA)
         THETA = ((2004.682 - 0.853*T0)*T - 0.426*T*T - 0.042*T**3)
     :           * RDSA
         A = COS(DJ)
         B = SIN(DJ)
         C = COS(AJ + ZETA)
         D = SIN(AJ + ZETA)
         E = COS(THETA)
         F = SIN(THETA)
         X = A*D
         Y = E*A*C - F*B
         ANGLE = 0.0
         IF(X.EQ.0.0.AND.Y.EQ.0.0)GO TO 100
         ANGLE = ATAN2(X,Y)
         DP = ASIN(E*B+F*A*C)
100      CONTINUE
         AP = ANGLE + ZED
         IF(AP.LT.0.0) AP = AP + TWOPI
      ELSE
*
*   If Initial & Final Epoch the Same
*   Do Nothing
*
         AP = AJ
         DP = DJ
      ENDIF
      END

