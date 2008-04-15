/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmi2c(double qfoo,double qbar,double qbaz,double Q0,
double qfobar,double*q1,double*q2){CIpars qfoobar;double Q3;
sbmc2ipa(qbaz,Q0,qfobar,&qfoobar,&Q3);sbmi2cqk(qfoo,qbar,&
qfoobar,q1,q2);}
