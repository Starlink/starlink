/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmamp(double qfoo,double qbar,double qbaz,double Q0,
double*qfobar,double*q1){double q2[21];sbmmappa(Q0,qbaz,q2);
sbmampqk(qfoo,qbar,q2,qfobar,q1);}
