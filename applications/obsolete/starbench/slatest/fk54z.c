/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmfk54z(double qfoo,double qbar,double qbaz,double*Q0,
double*qfobar,double*q1,double*q2){static double qfoobar=0.0
;double Q3,q4,qfOBAz,qfoobaz;sbmfk524(qfoo,qbar,qfoobar,
qfoobar,qfoobar,qfoobar,&Q3,&q4,q1,q2,&qfOBAz,&qfoobaz);
sbmpm(Q3,q4,*q1,*q2,qfoobar,qfoobar,1950.0,qbaz,Q0,qfobar);}
