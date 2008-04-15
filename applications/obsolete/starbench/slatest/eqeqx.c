/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
double sbmeqeqx(double qfoo)
#define qbar 1296000.0
#define qbaz 0.4848136811095359949E-5
{double Q0,qfobar,q1,q2,qfoobar;Q0=(qfoo-51544.5)/36525.0;
qfobar=qbaz*(450160.280+(-5.0*qbar-482890.539+(7.455+0.008*
Q0)*Q0)*Q0);sbmnutc(qfoo,&q1,&q2,&qfoobar);return q1*cos(
qfoobar)+qbaz*(0.00264*sin(qfobar)+0.000063*sin(qfobar+
qfobar));}
