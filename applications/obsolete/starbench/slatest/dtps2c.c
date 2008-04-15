/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdtps2c(double qfoo,double qbar,double qbaz,double Q0
,double*qfobar,double*q1,double*q2,double*qfoobar,int*Q3){
double q4,qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog,qcat,QFISH;q4=
qfoo*qfoo;qfOBAz=qbar*qbar;qfoobaz=sin(Q0);QQUUX=cos(Q0);Q5=
qfoobaz*sqrt(1.0+q4+qfOBAz);QFRED=QQUUX*QQUUX*(1.0+qfOBAz)-
qfoobaz*qfoobaz*q4;if(QFRED>=0.0){qdog=sqrt(QFRED);qcat=Q5-
qbar*qdog;QFISH=Q5*qbar+qdog;if(qfoo==0.0&&qdog==0.0)qdog=
1.0;*qfobar=sbmdranrm(qbaz-atan2(qfoo,qdog));*q1=atan2(qcat,
QFISH);qdog=-qdog;qcat=Q5-qbar*qdog;QFISH=Q5*qbar+qdog;*q2=
sbmdranrm(qbaz-atan2(qfoo,qdog));*qfoobar=atan2(qcat,QFISH);
*Q3=(fabs(Q5)<1.0)?1:2;}else{*Q3=0;}}
