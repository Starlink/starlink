/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmrefz(double qfoo,double refa,double refb,double*qbar
){double qbaz,Q0,qfobar,q1,q2,qfoobar,Q3,q4,qfOBAz,qfoobaz;
const double QQUUX=0.55445,Q5=-0.01133,QFRED=0.00202,qdog=
0.28385,qcat=0.02390;const double QFISH=93.0;const double
QgASp=83.0/DR2D;const double Q6=(QQUUX+Q5*7.0+QFRED*49.0)/(
1.0+qdog*7.0+qcat*49.0);qbaz=gmin(qfoo,QgASp);Q0=qbaz;qfobar
=sin(Q0);q1=cos(Q0);q2=qfobar/q1;qfoobar=q2*q2;Q3=q2*qfoobar
;Q0-=(refa*q2+refb*Q3)/(1.0+(refa+3.0*refb*qfoobar)/(q1*q1))
;qfobar=sin(Q0);q1=cos(Q0);q2=qfobar/q1;qfoobar=q2*q2;Q3=q2*
qfoobar;q4=qbaz-Q0+(Q0-qbaz+refa*q2+refb*Q3)/(1.0+(refa+3.0*
refb*qfoobar)/(q1*q1));if(qfoo>qbaz){qfOBAz=90.0-gmin(QFISH,
qfoo*DR2D);qfoobaz=qfOBAz*qfOBAz;q4=(q4/Q6)*(QQUUX+Q5*qfOBAz
+QFRED*qfoobaz)/(1.0+qdog*qfOBAz+qcat*qfoobaz);}*qbar=qfoo-
q4;}
