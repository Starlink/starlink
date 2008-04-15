/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdtp2s(double qfoo,double qbar,double qbaz,double Q0,
double*qfobar,double*q1){double q2,qfoobar,Q3;q2=sin(Q0);
qfoobar=cos(Q0);Q3=qfoobar-qbar*q2;*qfobar=sbmdranrm(atan2(
qfoo,Q3)+qbaz);*q1=atan2(q2+qbar*qfoobar,sqrt(qfoo*qfoo+Q3*
Q3));}
