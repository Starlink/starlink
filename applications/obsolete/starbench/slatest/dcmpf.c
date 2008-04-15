/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdcmpf(double qfoo[6],double*qbar,double*qbaz,double*
Q0,double*qfobar,double*q1,double*q2){double qfoobar,Q3,q4,
qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog,qcat,p,QFISH,QgASp,Q6,q7,
q8,QBAD,qBuG,qsilly,QBUGGY,QMUM,qDAd;qfoobar=qfoo[0];Q3=qfoo
[1];q4=qfoo[2];qfOBAz=qfoo[3];qfoobaz=qfoo[4];QQUUX=qfoo[5];
Q5=sqrt(Q3*Q3+qfoobaz*qfoobaz);QFRED=sqrt(q4*q4+QQUUX*QQUUX)
;if((Q3*QQUUX-q4*qfoobaz)>=0.0)qdog=Q5;else{Q3=-Q3;qfoobaz=-
qfoobaz;qdog=-Q5;}qcat=QFRED;p=sbmdrange(((q4!=0.0||QQUUX!=
0.0)?atan2(q4,QQUUX):0.0)+((qfoobaz!=0.0||Q3!=0.0)?atan2(
qfoobaz,Q3):0.0));QFISH=(q4*Q5)-(qfoobaz*QFRED);QgASp=(Q3*
QFRED)+(QQUUX*Q5);Q6=(QFISH!=0.0||QgASp!=0.0)?atan2(QFISH,
QgASp):0.0;q7=p/2.0;q8=sin(q7);QBAD=cos(q7);qBuG=sin(Q6);
qsilly=cos(Q6);QBUGGY=qdog*qcat*(QBAD+q8)*(QBAD-q8);if(fabs(
QBUGGY)>0.0){QMUM=qcat*(qfoobar*((QBAD*qsilly)-(q8*qBuG))-
qfOBAz*((QBAD*qBuG)+(q8*qsilly)))/QBUGGY;qDAd=qdog*(qfoobar*
((QBAD*qBuG)-(q8*qsilly))+qfOBAz*((QBAD*qsilly)+(q8*qBuG)))/
QBUGGY;}else{QMUM=0.0;qDAd=0.0;}*qbar=QMUM;*qbaz=qDAd;*Q0=
qdog;*qfobar=qcat;*q1=p;*q2=Q6;}
