      SUBROUTINE sla_EQECL (DR, DD, DATE, DL, DB)
*+
*     - - - - - -
*      E Q E C L
*     - - - - - -
*
*  Transformation from J2000.0 equatorial coordinates to
*  ecliptic coordinates (double precision)
*
*  Given:
*     DR,DD       dp      J2000.0 mean RA,Dec (radians)
*     DATE        dp      TDB (loosely ET) as Modified Julian Date
*                                              (JD-2400000.5)
*  Returned:
*     DL,DB       dp      ecliptic longitude and latitude
*                         (mean of date, IAU 1980 theory, radians)
*
*  Called:
*     sla_DCS2C, sla_PREC, sla_EPJ, sla_DMXV, sla_ECMAT, sla_DCC2S,
*     sla_DRANRM, sla_DRANGE
*
*  P.T.Wallace   Starlink   March 1986
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION DR,DD,DATE,DL,DB

      DOUBLE PRECISION sla_EPJ,sla_DRANRM,sla_DRANGE

      DOUBLE PRECISION RMAT(3,3),V1(3),V2(3)



*  Spherical to Cartesian
      CALL sla_DCS2C(DR,DD,V1)

*  Mean J2000 to mean of date
      CALL sla_PREC(2000D0,sla_EPJ(DATE),RMAT)
      CALL sla_DMXV(RMAT,V1,V2)

*  Equatorial to ecliptic
      CALL sla_ECMAT(DATE,RMAT)
      CALL sla_DMXV(RMAT,V2,V1)

*  Cartesian to spherical
      CALL sla_DCC2S(V1,DL,DB)

*  Express in conventional ranges
      DL=sla_DRANRM(DL)
      DB=sla_DRANGE(DB)

      END
