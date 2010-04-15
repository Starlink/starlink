      SUBROUTINE PROJ(N,RA,DEC,X,Y, STATUS )
*+
*   Routine for Converting from RA,Dec
*   to Plate X,Y Co-ords. and Vica-versa
*   Simple Tangential Plane Calculations
*
*   Gets
*   ----
*      N   -  = 1 if RA,Dec  to  X,Y
*             = 2 if  X,Y    tO  RA,Dec
*
*   Returns
*   -------
*      RA  -  Right Ascension in Radians
*      DEC -  Declination in Radians
*      X   -  X Co-ord. in Rads.
*      Y   -  Y Co-ord. in Rads.
*
*     STATUS = INTEGER (Given and Returned)
*   The global status.

*   Note - Plate Constants are in Common Block 'PCONST'
*
*   History:
*   Based on Routine in Program A06E Written by W. Nicholson
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*-

      IMPLICIT DOUBLE PRECISION(A-H,O-Z)

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      REAL X,Y
      COMMON/PCONST/SA,CA,SD,CD,CASD,SASD,CACD,SACD
      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF (N.LE.1) THEN
*
*   1.  Equatorial to Plate Co-ords.
*
         T  = COS(DEC)
         FL = T*COS(RA)
         FM = T*SIN(RA)
         FN = SIN(DEC)
         TL = -FL*SA   + FM*CA
         TM = -FL*CASD - FM*SASD + FN*CD
         TN =  FL*CACD + FM*SACD + FN*SD
         X  = REAL( TL/TN )
         Y  = REAL( TM/TN )

      ELSE
*
*   2.  Plate Co-ords to Equatorial
*
         PSI = X
         ETA = Y
         DIV = SQRT(1.0 + PSI*PSI * ETA*ETA)
         TN  = 1.0/DIV
         TM  = ETA*TN
         TL  = PSI*TN
         FL  = -TL*SA - TM*CASD + TN*CACD
         FM  =  TL*CA - TM*SASD + TN*SACD
         FN  =          TM*CD   + TN*SD
         A   = ATAN2(FM,FL) + TWOPI
         RA  = DMOD(A,TWOPI)
         T   = SQRT(FL*FL + FM*FM)
         DEC = ATAN2(FN,T)

      ENDIF

      END

