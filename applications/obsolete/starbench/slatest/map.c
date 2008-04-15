/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmmap(double qfoo,double qbar,double qbaz,double Q0,
double qfobar,double q1,double q2,double qfoobar,double*Q3,
double*q4){double qfOBAz[21];sbmmappa(q2,qfoobar,qfOBAz);
sbmmapqk(qfoo,qbar,qbaz,Q0,qfobar,q1,qfOBAz,Q3,q4);}
