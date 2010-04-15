      SUBROUTINE COORD(AP,DP,AO,DO,EQUOUT,GLAT,GLONG,ELAT,ELONG, STATUS)
*+
*      Compute Ecliptic and new Galactic Co-ordinates
*      from the Equatorial Co-ords
*
*      Gets
*      ----
*         AP,DP    - 1950 Equatorial Co-ords in Radians
*         AO,DO    - Equatorial Co-ords at Equinox EQUOUT
*         EQUOUT   - The Equinox of the Required Output Positions
*
*      Returns
*      -------
*         GLAT,GLONG  - New Galactic Latitude & Longitude (Degrees)
*         ELAT,ELONG  - Ecliptic Latitude & Longitude (Degrees)
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  History:
*     Sometime (UNK):
*        Original version.
*     2-MAR-1993 (AJJB):
*        STATUS argument added.
*     22-MAR-1993 (AJJB):
*        Commented out declaration of unused local variable, to keep
*        Sun complier quiet.
*-

*  Global constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      DOUBLE PRECISION TWOPI,HALFPI,RDSA,RDST,RDDG
      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG
      DOUBLE PRECISION AP,DP,AO,DO
      REAL GLAT,GLONG,ELAT,ELONG,OB,
     : DPGP,RPGP,LNP,EQUOUT
*     REAL OB1950

*+
*     Constants (in Degrees):-
*     OB1900    -  Obliquity of the Ecliptic 1900
*     DPGP      -  Dec (1950) of the Pole of the New Galactic Plane
*     RPGP      -  R.A. (1950) "  "    "   "  "   "     "       "
*     LNP       -  New Galactic Longitude of the North Celestial
*                  Pole (1950)
*-
      DATA OB1900,DPGP,RPGP,LNP/23.45229,27.40,192.25,123.00/

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CAP = REAL( COS(AP) )
      SAP = REAL( SIN(AP) )
      CDP = REAL( COS(DP) )
      SDP = REAL( SIN(DP) )
      CAO = REAL( COS(AO) )
      SAO = REAL( SIN(AO) )
      CDO = REAL( COS(DO) )
      SDO = REAL( SIN(DO) )
*
*     1. Ecliptic Co-ords
*        Referred to the Equinox and Obliquity of Date (EQUOUT)
*
      OB = REAL( OB1900*RDDG - ((46.85*RDSA)*((EQUOUT-1900.0)/100.0)) )
*
*      OB is Obliquity of the Ecliptic at EQUOUT
*
      SO = SIN(OB)
      CO = COS(OB)
      ELONG = REAL( ATAN2((CDO*SAO*CO)+(SDO*SO),CDO*CAO)/RDDG )
      ELONG = MOD(ELONG + 360.0,360.0)
      ELAT  = REAL( ASIN(SDO*CO - CDO*SAO*SO)/RDDG )
*
*     2. Galactic Co-ordinates
*
      CONSTA = REAL( AP - ((RPGP*RDDG) + HALFPI) )
      CONSTB = REAL( HALFPI - (DPGP * RDDG) )
      SCA = SIN(CONSTA)
      SCB = SIN(CONSTB)
      CCA = COS(CONSTA)
      CCB = COS(CONSTB)
      GLONG = REAL( ((LNP*RDDG - HALFPI) + ATAN2((CDP*SCA*CCB)+
     : (SDP*SCB),CDP*CCA))/RDDG )
      GLONG = MOD(GLONG+360.0,360.0)
      GLAT  = REAL( ASIN(-(CDP*SCA*SCB)+(SDP*CCB))/RDDG )
      END

