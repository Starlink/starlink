/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
double sbmdbear(double qfoo,double qbar,double qbaz,double
Q0){double qfobar,q1,q2;qfobar=qbaz-qfoo;q2=sin(qfobar)*cos(
Q0);q1=sin(Q0)*cos(qbar)-cos(Q0)*sin(qbar)*cos(qfobar);
return(q1!=0.0||q2!=0.0)?atan2(q2,q1):0.0;}
