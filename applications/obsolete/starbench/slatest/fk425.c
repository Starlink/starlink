/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmfk425(double qfoo,double qbar,double qbaz,double Q0,
double qfobar,double q1,double*q2,double*qfoobar,double*Q3,
double*q4,double*qfOBAz,double*qfoobaz){double QQUUX,Q5,
QFRED,qdog,qcat,QFISH,QgASp,Q6,q7,q8,QBAD,qBuG,qsilly,QBUGGY
,QMUM,qDAd,q9,Q10,Q11,q12,Q13,Q14,qdisk,Q15;int q16,q17;
double QEMPTY[3],q18[3];double QFULL[6],qfast[6];static
double qsmall=100.0*60.0*60.0*360.0/D2PI;double QBIG=1.0e-30
;double QOK=21.095;static double QHELLO[3]={-1.62557e-6,-
0.31919e-6,-0.13843e-6};static double QBYE[3]={1.245e-3,-
1.580e-3,-0.659e-3};static double QMAGIC[6][6]={{
0.9999256782,-0.0111820611,-0.0048579477,0.00000242395018,-
0.00000002710663,-0.00000001177656},{0.0111820610,
0.9999374784,-0.0000271765,0.00000002710663,0.00000242397878
,-0.00000000006587},{0.0048579479,-0.0000271474,0.9999881997
,0.00000001177656,-0.00000000006582,0.00000242410173},{-
0.000551,-0.238565,0.435739,0.99994704,-0.01118251,-
0.00485767},{0.238514,-0.002667,-0.008541,0.01118251,
0.99995883,-0.00002718},{-0.435623,0.012254,0.002117,
0.00485767,-0.00002714,1.00000956}};QQUUX=qfoo;Q5=qbar;QFRED
=qbaz*qsmall;qdog=Q0*qsmall;qcat=qfobar;QFISH=q1;QgASp=sin(
QQUUX);Q6=cos(QQUUX);q7=sin(Q5);q8=cos(Q5);QEMPTY[0]=Q6*q8;
QEMPTY[1]=QgASp*q8;QEMPTY[2]=q7;QBAD=QOK*QFISH*qcat;q18[0]=(
-QgASp*q8*QFRED)-(Q6*q7*qdog)+(QBAD*QEMPTY[0]);q18[1]=(Q6*q8
*QFRED)-(QgASp*q7*qdog)+(QBAD*QEMPTY[1]);q18[2]=(q8*qdog)+(
QBAD*QEMPTY[2]);QBAD=(QEMPTY[0]*QHELLO[0])+(QEMPTY[1]*QHELLO
[1])+(QEMPTY[2]*QHELLO[2]);qBuG=(QEMPTY[0]*QBYE[0])+(QEMPTY[
1]*QBYE[1])+(QEMPTY[2]*QBYE[2]);for(q16=0;q16<3;q16++){QFULL
[q16]=QEMPTY[q16]-QHELLO[q16]+QBAD*QEMPTY[q16];QFULL[q16+3]=
q18[q16]-QBYE[q16]+qBuG*QEMPTY[q16];}for(q16=0;q16<6;q16++){
QBAD=0.0;for(q17=0;q17<6;q17++){QBAD+=QMAGIC[q16][q17]*QFULL
[q17];}qfast[q16]=QBAD;}qsilly=qfast[0];QBUGGY=qfast[1];QMUM
=qfast[2];qDAd=qfast[3];q9=qfast[4];Q10=qfast[5];Q11=(qsilly
*qsilly)+(QBUGGY*QBUGGY);q12=(Q11)+(QMUM*QMUM);Q13=sqrt(Q11)
;Q14=sqrt(q12);qdisk=(qsilly*qDAd)+(QBUGGY*q9);Q15=qdisk+(
QMUM*Q10);QQUUX=(qsilly!=0.0||QBUGGY!=0.0)?atan2(QBUGGY,
qsilly):0.0;if(QQUUX<0.0)QQUUX+=D2PI;Q5=atan2(QMUM,Q13);if(
Q13>QBIG){QFRED=((qsilly*q9)-(QBUGGY*qDAd))/Q11;qdog=((Q10*
Q11)-(QMUM*qdisk))/(q12*Q13);}if(qcat>QBIG){QFISH=Q15/(qcat*
Q14*QOK);qcat=qcat/Q14;}*q2=QQUUX;*qfoobar=Q5;*Q3=QFRED/
qsmall;*q4=qdog/qsmall;*qfoobaz=QFISH;*qfOBAz=qcat;}
