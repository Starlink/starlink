/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmearth(int qfoo,int qbar,float qbaz,float Q0[6])
#define qfobar 1.9913e-7f
#define q1 3.12e-5f
#define q2 8.31e-11f
{float qfoobar,Q3,q4,qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog,qcat
,QFISH,QgASp,Q6,q7,q8,QBAD,qBuG,qsilly,QBUGGY,QMUM;int qDAd;
qfoobar=(float)(qfoo-1900);qDAd=qfoo>=0?qfoo%4:3-(-qfoo-1)%4
;Q3=((float)(4*(qbar-1/(qDAd+1))-qDAd-2)+4.0f*qbaz)/1461.0f;
q4=qfoobar+Q3;qfOBAz=(float)dmod(4.881628+D2PI*((double)Q3)+
0.00013420*((double)q4),D2PI);qfoobaz=4.908230f+3.0005e-4f*
q4;QQUUX=qfOBAz-qfoobaz;QFRED=0.40931975f-2.27e-6f*q4;qdog=
0.016751f-4.2e-7f*q4;qcat=(float)(qdog*qdog);QFISH=QQUUX+
2.0f*qdog*(float)sin((double)QQUUX)+1.25f*qcat*(float)sin(
2.0*(double)QQUUX);Q5=QFISH+qfoobaz;QgASp=(1.0f-qcat)/(1.0f+
qdog*(float)cos((double)QFISH));Q6=(float)dmod((4.72+83.9971
*((double)q4)),D2PI);q7=(float)cos((double)Q5);q8=(float)sin
((double)QFRED);QBAD=(float)cos((double)QFRED);qBuG=-QgASp*(
float)sin((double)Q5);qsilly=-qfobar*(q7+qdog*(float)cos((
double)qfoobaz));QBUGGY=(float)sin((double)Q6);QMUM=(float)
cos((double)Q6);Q0[0]=-QgASp*q7-q1*QMUM;Q0[1]=(qBuG-q1*
QBUGGY)*QBAD;Q0[2]=qBuG*q8;Q0[3]=qfobar*((float)sin((double)
Q5)+qdog*(float)sin((double)qfoobaz))+q2*QBUGGY;Q0[4]=(
qsilly-q2*QMUM)*QBAD;Q0[5]=qsilly*q8;}
