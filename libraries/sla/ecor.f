      SUBROUTINE sla_ECOR (RM, DM, IY, ID, FD, RV, TL)
*+
*     - - - - -
*      E C O R
*     - - - - -
*
*  Component of Earth orbit velocity and heliocentric
*  light time in a given direction (single precision)
*
*  Given:
*     RM,DM    real    mean RA, Dec of date (radians)
*     IY       int     year
*     ID       int     day in year (1 = Jan 1st)
*     FD       real    fraction of day
*
*  Returned:
*     RV       real    component of Earth orbital velocity (km/sec)
*     TL       real    component of heliocentric light time (sec)
*
*  Notes:
*
*  1  The date and time is TDB (loosely ET) in a Julian calendar
*     which has been aligned to the ordinary Gregorian
*     calendar for the interval 1900 March 1 to 2100 February 28.
*     The year and day can be obtained by calling sla_CALYD or
*     sla_CLYD.
*
*  2  Sign convention:
*
*     The velocity component is +ve when the Earth is receding from
*     the given point on the sky.  The light time component is +ve
*     when the Earth lies between the Sun and the given point on
*     the sky.
*
*  3  Accuracy:
*
*     The velocity component is usually within 0.004 km/s of the
*     correct value and is never in error by more than 0.007 km/s.
*     The error in light time correction is about 0.03s at worst,
*     but is usually better than 0.01s. For applications requiring
*     higher accuracy, see the sla_EVP routine.
*
*  Called:  sla_EARTH, sla_CS2C, sla_VDV
*
*  P.T.Wallace   Starlink   24 November 1994
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL RM,DM
      INTEGER IY,ID
      REAL FD,RV,TL

      REAL sla_VDV

      REAL PV(6),V(3),AUKM,AUSEC

*  AU to km and light sec (1985 Almanac)
      PARAMETER (AUKM=1.4959787066E8,
     :           AUSEC=499.0047837)



*  Sun:Earth position & velocity vector
      CALL sla_EARTH(IY,ID,FD,PV)

*  Star position vector
      CALL sla_CS2C(RM,DM,V)

*  Velocity component
      RV=-AUKM*sla_VDV(PV(4),V)

*  Light time component
      TL=AUSEC*sla_VDV(PV(1),V)

      END
