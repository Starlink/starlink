*+RO_SUNELONG    Returns long. of sun on ecliptic of date for given date(MJD)
      FUNCTION RO_SUNELONG(DMJD)
      IMPLICIT NONE

*  Calling Arguments
      DOUBLE PRECISION DMJD	! In	Date, MJD
      REAL RO_SUNELONG		! Out	Sun ecliptic long. radian
*  Author:   M J Ricketts   Jan 86,  from UK6LIB, consts from almanac
*-

      DOUBLE PRECISION DMJD0
      DOUBLE PRECISION W,W0,W1,W2,AM,AM0,AM1,AM2,E,E0,E1,D,DD,DD2,PHI,
     *     TH,TWOPI

      PARAMETER (DMJD0=15019.5D0, TWOPI=6.2830185D0,
     *           W0=4.9082296D0, W1=8.21499D-7, W2=5.917D-7,
     *           AM0=6.2565836D0, AM1=0.01720197D0, AM2=-1.95D-7,
     *           E0 = 0.01675104D0, E1 = -1.1444D-5 )

*  W is longitude of perigee, equinox of date
*  AM is mean anomaly,     E is eccentricity
*  W, AM and constants are in radians

      D =  DMJD - DMJD0
      DD = D * 1.0D-4
      DD2 = DD*DD


      W  = W0 + D*W1 + DD2*W2
      AM = AM0 + D*AM1 + DD2*AM2
      E  = E0 + DD*E1


      PHI = AM + E*SIN(AM) * (1.0D0 + E*COS(AM) )
      TH  = W + ATAN2( SIN(PHI) * SQRT( 1.0D0 - E*E ), COS(PHI)-E )


      RO_SUNELONG = DMOD(TH,TWOPI)

      END
