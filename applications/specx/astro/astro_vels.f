*  History:
*     22 Nov 1993 (hme):
*        Remove TABs.
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*     18 Sept 2000 (ajc):
*        SAVE ECCENT, EPSLON, SINEPS, COSEPS required for Linux
*        Unused COSLAT
C-----------------------------------------------------------------------

      SUBROUTINE ASTRO_VELS (DATJUL, APRA, APDEC, ALAT ,HA,
     &                       VSL, VES, VTE)

      IMPLICIT NONE
      REAL E, VXSUN, VYSUN, VZSUN, EQOLD, A1, A2, A3, A, B, C,
     :     EPSLON, EQ, SINEPS, COSEPS, VSUN, COSDEL, COSFAC,
     :     VXEARTH, VYEARTH, VZEARTH, SUNLON, ECCENT

C   Program to work out velocity of point on earth w.r.t the local
C   standard of rest.  Takes account of basic solar motion,
C   eccentricity of earth orbit and rotation of earth on axis.
C   Time series expansions obtained from astron.eph. or expl. supp a.e..
C
C   Formal parameters:
      DOUBLE PRECISION   DATJUL ! Modified julian date of observation
      REAL               APRA   ! R.A. of source at date
      REAL               APDEC  ! DEC.  "    "    "   "
      DOUBLE PRECISION   ALAT   ! Latitude of observatory
      REAL               HA     ! Hour angle of source
      REAL               VSL    ! Velocity of sun wrt LSR
      REAL               VES    ! Velocity of earth wrt sun
      REAL               VTE    ! Velocity of telescope wrt earth

C   Local variables:

      DOUBLE PRECISION   T
      DOUBLE PRECISION   RED
      DOUBLE PRECISION   PREMAT(3,3)
      DOUBLE PRECISION   RAAPX, DCAPX
      DOUBLE PRECISION   VX, VY, VZ

C   Functions:

      DOUBLE PRECISION   EPS
      DOUBLE PRECISION   ECC
      REAL               ECE

      COMMON /SUN/ VXSUN,VYSUN,VZSUN,A,B,C

      DATA EQOLD/0.0/

      SAVE ECCENT, EPSLON, SINEPS, COSEPS

C *** Set up statement functions

C (1) : ECE - correction to suns longitude for eccentricity of orbit

      ECE(E,A3)=E*(2.0*SIN(A3)+E*1.25*SIN(2.*A3)+E*E*(13./12.*SIN(3.
     &           *A3)-0.25*SIN(A3)))

C (2) : ECC - calculates eccentricity of earth orbit

      ECC(T)=0.01675104D0-(.00004180D0+.000000126D0*T)*T

C (3) : EPS - calculates obliquity of earths orbit

      EPS(T)=(84428.26D0-46.845D0*T-0.0059D0*T**2+0.00181D0*T**3)
     &         *4.8481368111D-06

C *** Calculate current epoch from Julian date
      T     = DATJUL/36525.0D0
      EQ    = 1950.0 + SNGL((DATJUL-18262.423D0) / 365.24219D0)
      IF (ABS(EQ-EQOLD).LE.0.25)   GO TO 30
      EQOLD = EQ

C *** Precess from 1950.0 to date ( i.e. calculate precession matrix )

      CALL PRE (1950.0, EQ, PREMAT)

C *** Evaluate relevant elements of earth's orbit at date

      ECCENT = ECC(T)
      EPSLON = RED(EPS(T))
      SINEPS = SIN(EPSLON)
      COSEPS = COS(EPSLON)

C *** Put in standard solar motion

      VSUN   = 20.0
      RAAPX  = 4.7207565308D0
      DCAPX  = 0.5236189365D0

C *** Convert apex to rectangular co-ordinates

      CALL CUV (RAAPX, DCAPX, VX, VY, VZ)

C *** Precess apex to present

      VXSUN = VSUN*(VX*PREMAT(1,1) + VY*PREMAT(1,2) + VZ*PREMAT(1,3))
      VYSUN = VSUN*(VX*PREMAT(2,1) + VY*PREMAT(2,2) + VZ*PREMAT(2,3))
      VZSUN = VSUN*(VX*PREMAT(3,1) + VY*PREMAT(3,2) + VZ*PREMAT(3,3))

C *** Calculate position of earth

   30 COSDEL = COS(APDEC)
      CALL A13 (DATJUL, A1, A2, A3)
      SUNLON = A1+ ECE (ECCENT,A3)

C *** Calculate velocity of centre of earth rel sun in eq.rectangulars

      COSFAC  = (COS(SUNLON)+ECCENT*COS(A2))*29.79
      VXEARTH = - ((SIN(SUNLON)+ECCENT*SIN(A2))*29.79)
      VYEARTH = + (COSFAC*COSEPS)
      VZEARTH = + (COSFAC*SINEPS)

C *** Now project both motions separately onto current ra,dec

      VES = VXEARTH*COSDEL*COS(APRA) + VYEARTH*COSDEL*SIN(APRA)
     &                               + VZEARTH*SIN(APDEC)

      VSL = - VXSUN*COSDEL*COS(APRA) - VYSUN*COSDEL*SIN(APRA)
     &                               - VZSUN*SIN(APDEC)

C *** Calculate radial component of rotation velocity of earth

      VTE = 0.465*SIN(HA)*COSDEL*COS(ALAT)

CD    PRINT *, ' -- astro_vels --'
CD    PRINT *, '     V(sun-lsr)   = ', VSL
CD    PRINT *, '     V(earth-sun) = ', VES
CD    PRINT *, '     V(tel-earth) = ', VTE
CD    PRINT *, '     (total vel)  = ', VTE + VES + VSL

      RETURN
      END

C-----------------------------------------------------------------------

