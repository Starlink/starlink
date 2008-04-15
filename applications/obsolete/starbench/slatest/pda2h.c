/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmpda2h(double p,double qfoo,double qbar,double*qbaz,
int*Q0,double*qfobar,int*q1)
#define q2 1e-12
{double qfoobar,Q3,q4,qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog,
qcat;*Q0=0;*q1=0;qfoobar=sbmdrange(p);if(fabs(fabs(qfoobar)-
DPIBY2)<q2){qfoobar-=dsign(q2,qfoobar);}else if(fabs(qfoobar
)<q2){qfoobar=q2;}Q3=sbmdrange(qbar);if(fabs(fabs(Q3)-DPI)<
q2){Q3-=dsign(q2,Q3);}else if(fabs(Q3)<q2){Q3=q2;}q4=
sbmdrange(qfoo);if(fabs(fabs(q4)-fabs(p))<q2){q4-=dsign(q2,
q4);}else if(fabs(fabs(q4)-DPIBY2)<q2){q4-=dsign(q2,q4);}
else if(fabs(q4)<q2){q4=q2;}qfOBAz=sin(Q3);qfoobaz=cos(Q3);
QQUUX=qfOBAz*sin(qfoobar);Q5=sin(q4)*qfOBAz*cos(qfoobar);
QFRED=cos(q4)*sqrt(qfoobaz*qfoobaz+QQUUX*QQUUX);if(fabs(Q5)
<=QFRED){qdog=asin(Q5/QFRED);qcat=atan2(QQUUX,-qfoobaz);*
qbaz=sbmdrange(qdog-qcat);*qfobar=sbmdrange(-qdog-(qcat+DPI)
);if(*qbaz*Q3>0.0)*Q0=-1;if(*qfobar*Q3>0.0)*q1=-1;}else{*Q0=
-1;*q1=-1;}}
