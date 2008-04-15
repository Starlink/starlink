/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmaoppa(double qfoo,double qbar,double qbaz,double Q0,
double hm,double qfobar,double q1,double q2,double qfoobar,
double rh,double wl,double tlr,double Q3[14])
#define q4 173.14463331
#define qfOBAz 1.00273790935
{double qfoobaz,QQUUX,Q5,QFRED,qdog,qcat,QFISH,QgASp,phi,Q6,
q7;qfoobaz=cos(Q0);QQUUX=cos(qbaz)*qfoobaz;Q5=sin(qbaz)*
qfoobaz;QFRED=sin(Q0);qdog=QQUUX-qfobar*QFRED;qcat=Q5+q1*
QFRED;QFISH=qfobar*QQUUX-q1*Q5+QFRED;QgASp=(qdog!=0.0||qcat
!=0.0)?atan2(qcat,qdog):0.0;phi=atan2(QFISH,sqrt(qdog*qdog+
qcat*qcat));Q3[0]=phi;Q3[1]=sin(phi);Q3[2]=cos(phi);sbmgeoc(
phi,hm,&Q6,&q7);Q3[3]=D2PI*Q6*qfOBAz/q4;Q3[4]=hm;Q3[5]=q2;Q3
[6]=qfoobar;Q3[7]=rh;Q3[8]=wl;Q3[9]=tlr;sbmrefco(hm,q2,
qfoobar,rh,wl,phi,tlr,1e-10,&Q3[10],&Q3[11]);Q3[12]=QgASp+
sbmeqeqx(qfoo)+qbar*qfOBAz*DS2R;sbmaoppat(qfoo,Q3);}
