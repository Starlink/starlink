      SUBROUTINE sla_EARTH (IY, ID, FD, PV)
*+
*     - - - - - -
*      E A R T H
*     - - - - - -
*
*  Approximate heliocentric position and velocity of the Earth
*
*  Given:
*     IY       I       year
*     ID       I       day in year (1 = Jan 1st)
*     FD       R       fraction of day
*
*  Returned:
*     PV       R(6)    Earth position & velocity vector
*
*  Notes:
*
*  1  The date and time is TDB (loosely ET) in a Julian calendar
*     which has been aligned to the ordinary Gregorian
*     calendar for the interval 1900 March 1 to 2100 February 28.
*     The year and day can be obtained by calling sla_CALYD or
*     sla_CLYD.
*
*  2  The Earth heliocentric 6-vector is mean equator and equinox
*     of date.  Position part, PV(1-3), is in AU;  velocity part,
*     PV(4-6), is in AU/sec.
*
*  3  Max/RMS errors 1950-2050:
*       13/5 E-5 AU = 19200/7600 km in position
*       47/26 E-10 AU/s = 0.0070/0.0039 km/s in speed
*
*  4  More precise results are obtainable with the routine sla_EVP.
*
*  P.T.Wallace   Starlink   23 November 1994
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      INTEGER IY,ID
      REAL FD,PV(6)

      INTEGER IY4
      REAL TWOPI,SPEED,REMB,SEMB,YI,YF,T,ELM,GAMMA,EM,ELT,EPS0,
     :     E,ESQ,V,R,ELMM,COSELT,SINEPS,COSEPS,W1,W2,SELMM,CELMM

      PARAMETER (TWOPI=6.28318530718)

*  Mean orbital speed of Earth, AU/s
      PARAMETER (SPEED=1.9913E-7)

*  Mean Earth:EMB distance and speed, AU and AU/s
      PARAMETER (REMB=3.12E-5,SEMB=8.31E-11)



*  Whole years & fraction of year, and years since J1900.0
      YI=FLOAT(IY-1900)
      IY4=MOD(MOD(IY,4)+4,4)
      YF=(FLOAT(4*(ID-1/(IY4+1))-IY4-2)+4.0*FD)/1461.0
      T=YI+YF

*  Geometric mean longitude of Sun
*  (cf 4.881627938+6.283319509911*T MOD 2PI)
      ELM=MOD(4.881628+TWOPI*YF+0.00013420*T,TWOPI)

*  Mean longitude of perihelion
      GAMMA=4.908230+3.0005E-4*T

*  Mean anomaly
      EM=ELM-GAMMA

*  Mean obliquity
      EPS0=0.40931975-2.27E-6*T

*  Eccentricity
      E=0.016751-4.2E-7*T
      ESQ=E*E

*  True anomaly
      V=EM+2.0*E*SIN(EM)+1.25*ESQ*SIN(2.0*EM)

*  True ecliptic longitude
      ELT=V+GAMMA

*  True distance
      R=(1.0-ESQ)/(1.0+E*COS(V))

*  Moon's mean longitude
      ELMM=MOD(4.72+83.9971*T,TWOPI)

*  Useful functions
      COSELT=COS(ELT)
      SINEPS=SIN(EPS0)
      COSEPS=COS(EPS0)
      W1=-R*SIN(ELT)
      W2=-SPEED*(COSELT+E*COS(GAMMA))
      SELMM=SIN(ELMM)
      CELMM=COS(ELMM)

*  Earth position and velocity
      PV(1)=-R*COSELT-REMB*CELMM
      PV(2)=(W1-REMB*SELMM)*COSEPS
      PV(3)=W1*SINEPS
      PV(4)=SPEED*(SIN(ELT)+E*SIN(GAMMA))+SEMB*SELMM
      PV(5)=(W2-SEMB*CELMM)*COSEPS
      PV(6)=W2*SINEPS

      END
