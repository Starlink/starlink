/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmunpcd(double qfoo,double*qbar,double*qbaz)
#define Q0 1.0/3.0
{double qfobar,q1,q2,qfoobar,Q3,q4,qfOBAz,qfoobaz,QQUUX,Q5,
QFRED,qdog,qcat,QFISH,QgASp,Q6,q7;qfobar=sqrt((*qbar)*(*qbar
)+(*qbaz)*(*qbaz));if(qfobar!=0.0&&qfoo!=0.0){q1=1.0/(3.0*
qfoo);q2=qfobar/(2.0*qfoo);Q3=q1*q1*q1+q2*q2;if(Q3>=0.0){
qfoobar=sqrt(Q3);Q3=q2+qfoobar;q4=pow(fabs(Q3),Q0);if(Q3<0.0
)q4=-q4;Q3=q2-qfoobar;qfOBAz=pow(fabs(Q3),Q0);if(Q3<0.0)
qfOBAz=-qfOBAz;qfoobaz=q4+qfOBAz;}else{Q3=2.0/sqrt(-3.0*qfoo
);QQUUX=4.0*qfobar/(qfoo*Q3*Q3*Q3);Q5=QQUUX*QQUUX;q4=Q5<1.0?
sqrt(1.0-Q5):0.0;QFRED=atan2(q4,QQUUX);qdog=Q3*cos((D2PI-
QFRED)/3.0);qcat=Q3*cos((QFRED)/3.0);QFISH=Q3*cos((D2PI+
QFRED)/3.0);QgASp=fabs(qdog-qfobar);Q6=fabs(qcat-qfobar);q7=
fabs(QFISH-qfobar);qfoobaz=QgASp<Q6?(QgASp<q7?qdog:QFISH):(
Q6<q7?qcat:QFISH);}qfoobaz/=qfobar;*qbar*=qfoobaz;*qbaz*=
qfoobaz;}}
