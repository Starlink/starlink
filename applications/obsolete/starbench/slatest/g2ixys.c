/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmg2ixys(double qfoo,double qbar,double qbaz,double Q0
[3][3]){double qfobar,q1,q2;qfobar=qfoo*qfoo+qbar*qbar;q1=
qfobar!=0.0?atan2(qbar,qfoo):0.0;q2=atan(sqrt(qfobar/(1.0-
qfobar)));sbmdeuler("\172\171\172",q1,q2,-(q1+qbaz),Q0);}
