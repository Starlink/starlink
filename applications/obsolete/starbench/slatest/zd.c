/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
double sbmzd(double qfoo,double qbar,double phi){double qbaz
,Q0,qfobar,q1,q2,qfoobar,Q3,q4,qfOBAz;qbaz=sin(qfoo);Q0=cos(
qfoo);qfobar=sin(qbar);q1=cos(qbar);q2=sin(phi);qfoobar=cos(
phi);Q3=Q0*q1*q2-qfobar*qfoobar;q4=qbaz*q1;qfOBAz=Q0*q1*
qfoobar+qfobar*q2;return atan2(sqrt(Q3*Q3+q4*q4),qfOBAz);}
