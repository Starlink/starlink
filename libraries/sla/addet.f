      SUBROUTINE sla_ADDET (RM, DM, EQ, RC, DC)
*+
*     - - - - - -
*      A D D E T
*     - - - - - -
*
*  Add the E-terms (elliptic component of annual aberration)
*  to a pre IAU 1976 mean place to conform to the old
*  catalogue convention (double precision)
*
*  Given:
*     RM,DM     dp     RA,Dec (radians) without E-terms
*     EQ        dp     Besselian epoch of mean equator and equinox
*
*  Returned:
*     RC,DC     dp     RA,Dec (radians) with E-terms included
*
*  Note:
*
*     Most star positions from pre-1984 optical catalogues (or
*     derived from astrometry using such stars) embody the
*     E-terms.  If it is necessary to convert a formal mean
*     place (for example a pulsar timing position) to one
*     consistent with such a star catalogue, then the RA,Dec
*     should be adjusted using this routine.
*
*  Reference:
*     Explanatory Supplement to the Astronomical Ephemeris,
*     section 2D, page 48.
*
*  Called:  sla_ETRMS, sla_DCS2C, sla_DCC2S, sla_DRANRM, sla_DRANGE
*
*  P.T.Wallace   Starlink   18 March 1999
*
*  Copyright (C) 1999 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION RM,DM,EQ,RC,DC

      DOUBLE PRECISION sla_DRANRM

      DOUBLE PRECISION A(3),V(3)

      INTEGER I



*  E-terms vector
      CALL sla_ETRMS(EQ,A)

*  Spherical to Cartesian
      CALL sla_DCS2C(RM,DM,V)

*  Include the E-terms
      DO I=1,3
         V(I)=V(I)+A(I)
      END DO

*  Cartesian to spherical
      CALL sla_DCC2S(V,RC,DC)

*  Bring RA into conventional range
      RC=sla_DRANRM(RC)

      END
