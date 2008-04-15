/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdh2e(double qfoo,double qbar,double phi,double*qbaz,
double*Q0){double qfobar,q1,q2,qfoobar,Q3,q4,qfOBAz,qfoobaz,
QQUUX,Q5;qfobar=sin(qfoo);q1=cos(qfoo);q2=sin(qbar);qfoobar=
cos(qbar);Q3=sin(phi);q4=cos(phi);qfOBAz=-q1*qfoobar*Q3+q2*
q4;qfoobaz=-qfobar*qfoobar;QQUUX=q1*qfoobar*q4+q2*Q3;Q5=sqrt
(qfOBAz*qfOBAz+qfoobaz*qfoobaz);*qbaz=(Q5!=0.0)?atan2(
qfoobaz,qfOBAz):0.0;*Q0=atan2(QQUUX,Q5);}
