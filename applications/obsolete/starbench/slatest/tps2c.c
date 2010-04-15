/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmtps2c(float qfoo,float qbar,float qbaz,float Q0,
float*qfobar,float*q1,float*q2,float*qfoobar,int*Q3){float
q4,qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog,qcat,QFISH;q4=qfoo*
qfoo;qfOBAz=qbar*qbar;qfoobaz=(float)sin((double)Q0);QQUUX=(
float)cos((double)Q0);Q5=qfoobaz*(float)sqrt((double)(1.0f+
q4+qfOBAz));QFRED=QQUUX*QQUUX*(1.0f+qfOBAz)-qfoobaz*qfoobaz*
q4;if(QFRED>=0.0f){qdog=(float)sqrt((double)QFRED);qcat=Q5-
qbar*qdog;QFISH=Q5*qbar+qdog;if(qfoo==0.0f&&qdog==0.0f){qdog
=1.0f;}*qfobar=sbmranorm(qbaz-(float)atan2((double)qfoo,(
double)qdog));*q1=(float)atan2((double)qcat,(double)QFISH);
qdog=-qdog;qcat=Q5-qbar*qdog;QFISH=Q5*qbar+qdog;*q2=
sbmranorm(qbaz-(float)atan2((double)qfoo,(double)qdog));*
qfoobar=(float)atan2((double)qcat,(double)QFISH);*Q3=(fabs((
double)Q5)<1.0)?1:2;}else{*Q3=0;}}
