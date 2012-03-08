/*
*+
*  Name:
*     palDmoon

*  Purpose:
*     Approximate geocentric position and velocity of the Moon

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palDmoon( double date, double pv[6] );

*  Arguments:
*     date = double (Given)
*        TDB as a Modified Julian Date (JD-2400000.5)
*     pv = double [6] (Returned)
*        Moon x,y,z,xdot,ydot,zdot, mean equator and
*        equinox of date (AU, AU/s)

*  Description:
*      Calculate the approximate geocentric position of the Moon
*      using a full implementation of the algorithm published by
*      Meeus (l'Astronomie, June 1984, p348).

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Meeus quotes accuracies of 10 arcsec in longitude, 3 arcsec in
*       latitude and 0.2 arcsec in HP (equivalent to about 20 km in
*       distance).  Comparison with JPL DE200 over the interval
*       1960-2025 gives RMS errors of 3.7 arcsec and 83 mas/hour in
*       longitude, 2.3 arcsec and 48 mas/hour in latitude, 11 km
*       and 81 mm/s in distance.  The maximum errors over the same
*       interval are 18 arcsec and 0.50 arcsec/hour in longitude,
*       11 arcsec and 0.24 arcsec/hour in latitude, 40 km and 0.29 m/s
*       in distance.
*     - The original algorithm is expressed in terms of the obsolete
*       timescale Ephemeris Time.  Either TDB or TT can be used, but
*       not UT without incurring significant errors (30 arcsec at
*       the present time) due to the Moon's 0.5 arcsec/sec movement.
*     - The algorithm is based on pre IAU 1976 standards.  However,
*       the result has been moved onto the new (FK5) equinox, an
*       adjustment which is in any case much smaller than the
*       intrinsic accuracy of the procedure.
*     - Velocity is obtained by a complete analytical differentiation
*       of the Meeus model.

*  History:
*     2012-03-07 (TIMJ):
*        Initial version based on a direct port of the SLA/F code.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"
#include "sofam.h"

/* Autoconf can give us -DPIC */
#undef PIC

void palDmoon( double date, double pv[6] ) {

  /*  Seconds per Julian century (86400*36525) */
  const double CJ = 3155760000.0;

  /*  Julian epoch of B1950 */
  const double B1950 = 1949.9997904423;

  /*  Earth equatorial radius in AU ( = 6378.137 / 149597870 ) */
  const double ERADAU=4.2635212653763e-5;

  double T,THETA,SINOM,COSOM,DOMCOM,WA,DWA,WB,DWB,WOM,
    DWOM,SINWOM,COSWOM,V,DV,COEFF,EMN,EMPN,DN,FN,EN,
    DEN,DTHETA,FTHETA,EL,DEL,B,DB,BF,DBF,P,DP,SP,R,
    DR,X,Y,Z,XD,YD,ZD,SEL,CEL,SB,CB,RCB,RBD,W,EPJ,
    EQCOR,EPS,SINEPS,COSEPS,ES,EC;
  double ELP, DELP;
  double EM, DEM, EMP, DEMP, D, DD, F, DF, OM, DOM, E, DESQ, ESQ, DE;
  int N,I;

  /*
   *  Coefficients for fundamental arguments
   *
   *   at J1900:  T**0, T**1, T**2, T**3
   *   at epoch:  T**0, T**1
   *
   *  Units are degrees for position and Julian centuries for time
   *
   */

  /*  Moon's mean longitude */
  const double ELP0=270.434164;
  const double ELP1=481267.8831;
  const double ELP2=-0.001133;
  const double ELP3=0.0000019;

  /*  Sun's mean anomaly */
  const double EM0=358.475833;
  const double EM1=35999.0498;
  const double EM2=-0.000150;
  const double EM3=-0.0000033;

  /*  Moon's mean anomaly */
  const double EMP0=296.104608;
  const double EMP1=477198.8491;
  const double EMP2=0.009192;
  const double EMP3=0.0000144;

  /*  Moon's mean elongation */
  const double D0=350.737486;
  const double D1=445267.1142;
  const double D2=-0.001436;
  const double D3=0.0000019;

  /*  Mean distance of the Moon from its ascending node */
  const double F0=11.250889;
  const double F1=483202.0251;
  const double F2=-0.003211;
  const double F3=-0.0000003;

  /*  Longitude of the Moon's ascending node */
  const double OM0=259.183275;
  const double OM1=-1934.1420;
  const double OM2=0.002078;
  const double OM3=0.0000022;

  /*  Coefficients for (dimensionless) E factor */
  const double E1=-0.002495;
  const double E2=-0.00000752;

  /*  Coefficients for periodic variations etc */
  const double PAC=0.000233;
  const double PA0=51.2;
  const double PA1=20.2;
  const double PBC=-0.001778;
  const double PCC=0.000817;
  const double PDC=0.002011;
  const double PEC=0.003964;
  const double PE0=346.560;
  const double PE1=132.870;
  const double PE2=-0.0091731;
  const double PFC=0.001964;
  const double PGC=0.002541;
  const double PHC=0.001964;
  const double PIC=-0.024691;
  const double PJC=-0.004328;
  const double PJ0=275.05;
  const double PJ1=-2.30;
  const double CW1=0.0004664;
  const double CW2=0.0000754;

  /*
   *  Coefficients for Moon position
   *
   *   Tx(N)       = coefficient of L, B or P term (deg)
   *   ITx(N,1-5)  = coefficients of M, M', D, F, E**n in argument
   */
#define NL 50
#define NB 45
#define NP 31

  /*
   *  Longitude
   */
  const double TL[NL] = {
    6.28875,1.274018,.658309,.213616,-.185596,
    -.114336,.058793,.057212,.05332,.045874,.041024,-.034718,-.030465,
    .015326,-.012528,-.01098,.010674,.010034,.008548,-.00791,-.006783,
    .005162,.005,.004049,.003996,.003862,.003665,.002695,.002602,
    .002396,-.002349,.002249,-.002125,-.002079,.002059,-.001773,
    -.001595,.00122,-.00111,8.92e-4,-8.11e-4,7.61e-4,7.17e-4,7.04e-4,
    6.93e-4,5.98e-4,5.5e-4,5.38e-4,5.21e-4,4.86e-4
  };

  const int ITL[NL][5] = {
    /* M   M'  D   F   n */
    { +0, +1, +0, +0, 0 },
    { +0, -1, +2, +0, 0 },
    { +0, +0, +2, +0, 0 },
    { +0, +2, +0, +0, 0 },
    { +1, +0, +0, +0, 1 },
    { +0, +0, +0, +2, 0 },
    { +0, -2, +2, +0, 0 },
    { -1, -1, +2, +0, 1 },
    { +0, +1, +2, +0, 0 },
    { -1, +0, +2, +0, 1 },
    { -1, +1, +0, +0, 1 },
    { +0, +0, +1, +0, 0 },
    { +1, +1, +0, +0, 1 },
    { +0, +0, +2, -2, 0 },
    { +0, +1, +0, +2, 0 },
    { +0, -1, +0, +2, 0 },
    { +0, -1, +4, +0, 0 },
    { +0, +3, +0, +0, 0 },
    { +0, -2, +4, +0, 0 },
    { +1, -1, +2, +0, 1 },
    { +1, +0, +2, +0, 1 },
    { +0, +1, -1, +0, 0 },
    { +1, +0, +1, +0, 1 },
    { -1, +1, +2, +0, 1 },
    { +0, +2, +2, +0, 0 },
    { +0, +0, +4, +0, 0 },
    { +0, -3, +2, +0, 0 },
    { -1, +2, +0, +0, 1 },
    { +0, +1, -2, -2, 0 },
    { -1, -2, +2, +0, 1 },
    { +0, +1, +1, +0, 0 },
    { -2, +0, +2, +0, 2 },
    { +1, +2, +0, +0, 1 },
    { +2, +0, +0, +0, 2 },
    { -2, -1, +2, +0, 2 },
    { +0, +1, +2, -2, 0 },
    { +0, +0, +2, +2, 0 },
    { -1, -1, +4, +0, 1 },
    { +0, +2, +0, +2, 0 },
    { +0, +1, -3, +0, 0 },
    { +1, +1, +2, +0, 1 },
    { -1, -2, +4, +0, 1 },
    { -2, +1, +0, +0, 2 },
    { -2, +1, -2, +0, 2 },
    { +1, -2, +2, +0, 1 },
    { -1, +0, +2, -2, 1 },
    { +0, +1, +4, +0, 0 },
    { +0, +4, +0, +0, 0 },
    { -1, +0, +4, +0, 1 },
    { +0, +2, -1, +0, 0 }
  };

  /*
   *  Latitude
   */
  const double TB[NB] = {
    5.128189,.280606,.277693,.173238,.055413,
    .046272,.032573,.017198,.009267,.008823,.008247,.004323,.0042,
    .003372,.002472,.002222,.002072,.001877,.001828,-.001803,-.00175,
    .00157,-.001487,-.001481,.001417,.00135,.00133,.001106,.00102,
    8.33e-4,7.81e-4,6.7e-4,6.06e-4,5.97e-4,4.92e-4,4.5e-4,4.39e-4,
    4.23e-4,4.22e-4,-3.67e-4,-3.53e-4,3.31e-4,3.17e-4,3.06e-4,
    -2.83e-4
  };

  const int ITB[NB][5] = {
    /* M   M'  D   F   n */
    { +0, +0, +0, +1, 0 },
    { +0, +1, +0, +1, 0 },
    { +0, +1, +0, -1, 0 },
    { +0, +0, +2, -1, 0 },
    { +0, -1, +2, +1, 0 },
    { +0, -1, +2, -1, 0 },
    { +0, +0, +2, +1, 0 },
    { +0, +2, +0, +1, 0 },
    { +0, +1, +2, -1, 0 },
    { +0, +2, +0, -1, 0 },
    { -1, +0, +2, -1, 1 },
    { +0, -2, +2, -1, 0 },
    { +0, +1, +2, +1, 0 },
    { -1, +0, -2, +1, 1 },
    { -1, -1, +2, +1, 1 },
    { -1, +0, +2, +1, 1 },
    { -1, -1, +2, -1, 1 },
    { -1, +1, +0, +1, 1 },
    { +0, -1, +4, -1, 0 },
    { +1, +0, +0, +1, 1 },
    { +0, +0, +0, +3, 0 },
    { -1, +1, +0, -1, 1 },
    { +0, +0, +1, +1, 0 },
    { +1, +1, +0, +1, 1 },
    { -1, -1, +0, +1, 1 },
    { -1, +0, +0, +1, 1 },
    { +0, +0, -1, +1, 0 },
    { +0, +3, +0, +1, 0 },
    { +0, +0, +4, -1, 0 },
    { +0, -1, +4, +1, 0 },
    { +0, +1, +0, -3, 0 },
    { +0, -2, +4, +1, 0 },
    { +0, +0, +2, -3, 0 },
    { +0, +2, +2, -1, 0 },
    { -1, +1, +2, -1, 1 },
    { +0, +2, -2, -1, 0 },
    { +0, +3, +0, -1, 0 },
    { +0, +2, +2, +1, 0 },
    { +0, -3, +2, -1, 0 },
    { +1, -1, +2, +1, 1 },
    { +1, +0, +2, +1, 1 },
    { +0, +0, +4, +1, 0 },
    { -1, +1, +2, +1, 1 },
    { -2, +0, +2, -1, 2 },
    { +0, +1, +0, +3, 0 }
  };

  /*
   *  Parallax
   */
  const double TP[NP] = {
    .950724,.051818,.009531,.007843,.002824,
    8.57e-4,5.33e-4,4.01e-4,3.2e-4,-2.71e-4,-2.64e-4,-1.98e-4,1.73e-4,
    1.67e-4,-1.11e-4,1.03e-4,-8.4e-5,-8.3e-5,7.9e-5,7.2e-5,6.4e-5,
    -6.3e-5,4.1e-5,3.5e-5,-3.3e-5,-3e-5,-2.9e-5,-2.9e-5,2.6e-5,
    -2.3e-5,1.9e-5
  };

  const int ITP[NP][5] = {
    /* M   M'  D   F   n */
    { +0, +0, +0, +0, 0 },
    { +0, +1, +0, +0, 0 },
    { +0, -1, +2, +0, 0 },
    { +0, +0, +2, +0, 0 },
    { +0, +2, +0, +0, 0 },
    { +0, +1, +2, +0, 0 },
    { -1, +0, +2, +0, 1 },
    { -1, -1, +2, +0, 1 },
    { -1, +1, +0, +0, 1 },
    { +0, +0, +1, +0, 0 },
    { +1, +1, +0, +0, 1 },
    { +0, -1, +0, +2, 0 },
    { +0, +3, +0, +0, 0 },
    { +0, -1, +4, +0, 0 },
    { +1, +0, +0, +0, 1 },
    { +0, -2, +4, +0, 0 },
    { +0, +2, -2, +0, 0 },
    { +1, +0, +2, +0, 1 },
    { +0, +2, +2, +0, 0 },
    { +0, +0, +4, +0, 0 },
    { -1, +1, +2, +0, 1 },
    { +1, -1, +2, +0, 1 },
    { +1, +0, +1, +0, 1 },
    { -1, +2, +0, +0, 1 },
    { +0, +3, -2, +0, 0 },
    { +0, +1, +1, +0, 0 },
    { +0, +0, -2, +2, 0 },
    { +1, +2, +0, +0, 1 },
    { -2, +0, +2, +0, 2 },
    { +0, +1, -2, +2, 0 },
    { -1, -1, +4, +0, 1 }
  };

  /*  Centuries since J1900 */
  T=(date-15019.5)/36525.;

  /*
   *  Fundamental arguments (radians) and derivatives (radians per
   *  Julian century) for the current epoch
   */

  /*  Moon's mean longitude */
  ELP=DD2R*fmod(ELP0+(ELP1+(ELP2+ELP3*T)*T)*T,360.);
  DELP=DD2R*(ELP1+(2.*ELP2+3*ELP3*T)*T);

  /*  Sun's mean anomaly */
  EM=DD2R*fmod(EM0+(EM1+(EM2+EM3*T)*T)*T,360.);
  DEM=DD2R*(EM1+(2.*EM2+3*EM3*T)*T);

  /*  Moon's mean anomaly */
  EMP=DD2R*fmod(EMP0+(EMP1+(EMP2+EMP3*T)*T)*T,360.);
  DEMP=DD2R*(EMP1+(2.*EMP2+3*EMP3*T)*T);

  /*  Moon's mean elongation */
  D=DD2R*fmod(D0+(D1+(D2+D3*T)*T)*T,360.);
  DD=DD2R*(D1+(2.*D2+3.*D3*T)*T);

  /*  Mean distance of the Moon from its ascending node */
  F=DD2R*fmod(F0+(F1+(F2+F3*T)*T)*T,360.);
  DF=DD2R*(F1+(2.*F2+3.*F3*T)*T);

  /*  Longitude of the Moon's ascending node */
  OM=DD2R*fmod(OM0+(OM1+(OM2+OM3*T)*T)*T,360.);
  DOM=DD2R*(OM1+(2.*OM2+3.*OM3*T)*T);
  SINOM=sin(OM);
  COSOM=cos(OM);
  DOMCOM=DOM*COSOM;

  /*  Add the periodic variations */
  THETA=DD2R*(PA0+PA1*T);
  WA=sin(THETA);
  DWA=DD2R*PA1*cos(THETA);
  THETA=DD2R*(PE0+(PE1+PE2*T)*T);
  WB=PEC*sin(THETA);
  DWB=DD2R*PEC*(PE1+2.*PE2*T)*cos(THETA);
  ELP=ELP+DD2R*(PAC*WA+WB+PFC*SINOM);
  DELP=DELP+DD2R*(PAC*DWA+DWB+PFC*DOMCOM);
  EM=EM+DD2R*PBC*WA;
  DEM=DEM+DD2R*PBC*DWA;
  EMP=EMP+DD2R*(PCC*WA+WB+PGC*SINOM);
  DEMP=DEMP+DD2R*(PCC*DWA+DWB+PGC*DOMCOM);
  D=D+DD2R*(PDC*WA+WB+PHC*SINOM);
  DD=DD+DD2R*(PDC*DWA+DWB+PHC*DOMCOM);
  WOM=OM+DD2R*(PJ0+PJ1*T);
  DWOM=DOM+DD2R*PJ1;
  SINWOM=sin(WOM);
  COSWOM=cos(WOM);
  F=F+DD2R*(WB+PIC*SINOM+PJC*SINWOM);
  DF=DF+DD2R*(DWB+PIC*DOMCOM+PJC*DWOM*COSWOM);

  /*  E-factor, and square */
  E=1.+(E1+E2*T)*T;
  DE=E1+2.*E2*T;
  ESQ=E*E;
  DESQ=2.*E*DE;

  /*
   *  Series expansions
   */

  /*  Longitude */
  V=0.;
  DV=0.;
  for (N=NL-1; N>=0; N--) { /* DO N=NL, 1, -1 */
    COEFF=TL[N];
    EMN=(double)(ITL[N][0]);
    EMPN=(double)(ITL[N][1]);
    DN=(double)(ITL[N][2]);
    FN=(double)(ITL[N][3]);
    I=ITL[N][4];
    if (I == 0) {
      EN=1.;
      DEN=0.;
    } else if (I == 1) {
      EN=E;
      DEN=DE;
    } else {
      EN=ESQ;
      DEN=DESQ;
    }
    THETA=EMN*EM+EMPN*EMP+DN*D+FN*F;
    DTHETA=EMN*DEM+EMPN*DEMP+DN*DD+FN*DF;
    FTHETA=sin(THETA);
    V=V+COEFF*FTHETA*EN;
    DV=DV+COEFF*(cos(THETA)*DTHETA*EN+FTHETA*DEN);
  }
  EL=ELP+DD2R*V;
  DEL=(DELP+DD2R*DV)/CJ;

  /*  Latitude */
  V=0.;
  DV=0.;
  for (N=NB-1; N>=0; N--) { /* DO N=NB,1,-1 */
    COEFF=TB[N];
    EMN=(double)(ITB[N][0]);
    EMPN=(double)(ITB[N][1]);
    DN=(double)(ITB[N][2]);
    FN=(double)(ITB[N][3]);
    I=ITB[N][4];
    if (I == 0 ) {
      EN=1.;
      DEN=0.;
    } else if (I == 1) {
      EN=E;
      DEN=DE;
    } else {
      EN=ESQ;
      DEN=DESQ;
    }
    THETA=EMN*EM+EMPN*EMP+DN*D+FN*F;
    DTHETA=EMN*DEM+EMPN*DEMP+DN*DD+FN*DF;
    FTHETA=sin(THETA);
    V=V+COEFF*FTHETA*EN;
    DV=DV+COEFF*(cos(THETA)*DTHETA*EN+FTHETA*DEN);
  }
  BF=1.-CW1*COSOM-CW2*COSWOM;
  DBF=CW1*DOM*SINOM+CW2*DWOM*SINWOM;
  B=DD2R*V*BF;
  DB=DD2R*(DV*BF+V*DBF)/CJ;

  /*  Parallax */
  V=0.;
  DV=0.;
  for (N=NP-1; N>=0; N--) { /* DO N=NP,1,-1 */
    COEFF=TP[N];
    EMN=(double)(ITP[N][0]);
    EMPN=(double)(ITP[N][1]);
    DN=(double)(ITP[N][2]);
    FN=(double)(ITP[N][3]);
    I=ITP[N][4];
    if (I == 0) {
      EN=1.;
      DEN=0.;
    } else if (I == 1) {
      EN=E;
      DEN=DE;
    } else {
      EN=ESQ;
      DEN=DESQ;
    }
    THETA=EMN*EM+EMPN*EMP+DN*D+FN*F;
    DTHETA=EMN*DEM+EMPN*DEMP+DN*DD+FN*DF;
    FTHETA=cos(THETA);
    V=V+COEFF*FTHETA*EN;
    DV=DV+COEFF*(-sin(THETA)*DTHETA*EN+FTHETA*DEN);
  }
  P=DD2R*V;
  DP=DD2R*DV/CJ;

  /*
   *  Transformation into final form
   */

  /*  Parallax to distance (AU, AU/sec) */
  SP=sin(P);
  R=ERADAU/SP;
  DR=-R*DP*cos(P)/SP;

  /*  Longitude, latitude to x,y,z (AU) */
  SEL=sin(EL);
  CEL=cos(EL);
  SB=sin(B);
  CB=cos(B);
  RCB=R*CB;
  RBD=R*DB;
  W=RBD*SB-CB*DR;
  X=RCB*CEL;
  Y=RCB*SEL;
  Z=R*SB;
  XD=-Y*DEL-W*CEL;
  YD=X*DEL-W*SEL;
  ZD=RBD*CB+SB*DR;

  /*  Julian centuries since J2000 */
  T=(date-51544.5)/36525.;

  /*  Fricke equinox correction */
  EPJ=2000.+T*100.;
  EQCOR=DS2R*(0.035+0.00085*(EPJ-B1950));

  /*  Mean obliquity (IAU 1976) */
  EPS=DAS2R*(84381.448+(-46.8150+(-0.00059+0.001813*T)*T)*T);

  /*  To the equatorial system, mean of date, FK5 system */
  SINEPS=sin(EPS);
  COSEPS=cos(EPS);
  ES=EQCOR*SINEPS;
  EC=EQCOR*COSEPS;
  pv[0]=X-EC*Y+ES*Z;
  pv[1]=EQCOR*X+Y*COSEPS-Z*SINEPS;
  pv[2]=Y*SINEPS+Z*COSEPS;
  pv[3]=XD-EC*YD+ES*ZD;
  pv[4]=EQCOR*XD+YD*COSEPS-ZD*SINEPS;
  pv[5]=YD*SINEPS+ZD*COSEPS;

}
