/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdtpv2c(double qfoo,double qbar,double qbaz[3],double
 Q0[3],double qfobar[3],int*q1){double q2,qfoobar,Q3,q4,
qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog;q2=qbaz[0];qfoobar=qbaz[1
];Q3=qbaz[2];q4=q2*q2+qfoobar*qfoobar;qfOBAz=qfoo*qfoo;
qfoobaz=qbar*qbar+1.0;QQUUX=Q3*sqrt(qfOBAz+qfoobaz);Q5=q4*
qfoobaz-Q3*Q3*qfOBAz;if(Q5>0.0){QFRED=sqrt(Q5);qdog=(QQUUX*
qbar+QFRED)/(qfoobaz*sqrt(q4*(Q5+qfOBAz)));Q0[0]=qdog*(q2*
QFRED+qfoobar*qfoo);Q0[1]=qdog*(qfoobar*QFRED-q2*qfoo);Q0[2]
=(QQUUX-qbar*QFRED)/qfoobaz;QFRED=-QFRED;qdog=(QQUUX*qbar+
QFRED)/(qfoobaz*sqrt(q4*(Q5+qfOBAz)));qfobar[0]=qdog*(q2*
QFRED+qfoobar*qfoo);qfobar[1]=qdog*(qfoobar*QFRED-q2*qfoo);
qfobar[2]=(QQUUX-qbar*QFRED)/qfoobaz;*q1=(fabs(QQUUX)<1.0)?1
:2;}else{*q1=0;}}
