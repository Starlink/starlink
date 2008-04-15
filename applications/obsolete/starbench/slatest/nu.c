/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmnu(double qfoo,double*qbar,double*qbaz)
#define Q0 36525.0
#define qfobar 51544.5
{double q1,q2,qfoobar,Q3;q1=(qfoo-qfobar)/Q0;q2=-2.7774e-6*
q1;sbmnu00a(qfoo,&qfoobar,&Q3);*qbar=qfoobar+qfoobar*(
0.4697e-6+q2);*qbaz=Q3+Q3*q2;}
