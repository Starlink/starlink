/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
double sbmdsep(double qfoo,double qbar,double qbaz,double Q0
){double qfobar[3],q1[3];sbmdcs2c(qfoo,qbar,qfobar);sbmdcs2c
(qbaz,Q0,q1);return sbmdsepv(qfobar,q1);}
