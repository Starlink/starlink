/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmaltaz(double qfoo,double qbar,double phi,double*qbaz
,double*Q0,double*qfobar,double*q1,double*q2,double*qfoobar,
double*Q3,double*q4,double*qfOBAz)
#define qfoobaz 1e-30
{double QQUUX,Q5,QFRED,qdog,qcat,QFISH,QgASp,Q6,q7,q8,QBAD,
qBuG,qsilly,QBUGGY,QMUM,qDAd,q9,Q10,Q11,q12,Q13,Q14,qdisk,
Q15,q16;QQUUX=sin(qfoo);Q5=cos(qfoo);QFRED=sin(qbar);qdog=
cos(qbar);qcat=sin(phi);QFISH=cos(phi);QgASp=Q5*qdog;Q6=
QFRED*QFISH;q7=-QgASp*qcat+Q6;q8=-QQUUX*qdog;QBAD=QgASp*
QFISH+QFRED*qcat;qBuG=q7*q7+q8*q8;qsilly=sqrt(qBuG);QBUGGY=
qBuG!=0.0?atan2(q8,q7):0.0;if(QBUGGY<0.0)QBUGGY+=D2PI;QMUM=
atan2(QBAD,qsilly);qDAd=qdog*qcat-Q5*Q6;q9=QQUUX*QFISH;Q10=(
q9!=0.0||qDAd!=0.0)?atan2(q9,qDAd):DPI-qfoo;if(qBuG<qfoobaz)
{qBuG=qfoobaz;qsilly=sqrt(qBuG);}Q11=-q7*QFISH/qBuG;q12=qcat
+QBAD*Q11;Q13=QFISH*q8/qsilly;Q14=Q13/qsilly;qdisk=Q14*(QBAD
*qcat+(2.0-qBuG)*Q11);Q15=-qsilly*Q11*q12;q16=Q14*(qcat+2.0*
QBAD*Q11);*qbaz=QBUGGY;*Q0=q12;*qfobar=qdisk;*q1=QMUM;*q2=
Q13;*qfoobar=Q15;*Q3=Q10;*q4=Q11;*qfOBAz=q16;}
