/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmrefco(double hm,double qfoo,double qbar,double rh,
double wl,double phi,double tlr,double qbaz,double*refa,
double*refb){double Q0,qfobar;static double q1=
0.7853981633974483;static double q2=1.325817663668033;
sbmrefro(q1,hm,qfoo,qbar,rh,wl,phi,tlr,qbaz,&Q0);sbmrefro(q2
,hm,qfoo,qbar,rh,wl,phi,tlr,qbaz,&qfobar);*refa=(64.0*Q0-
qfobar)/60.0;*refb=(qfobar-4.0*Q0)/60.0;}
