/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmi2o(double qfoo,double qbar,double qbaz,double Q0,
double qfobar,double phi,double hm,double q1,double q2,
double tk,double qfoobar,double rh,double wl,double tlr,
double*Q3,double*q4,double*qfOBAz,double*qfoobaz,double*
QQUUX){IOpars Q5;sbmi2opa(qbaz,Q0,qfobar,phi,hm,q1,q2,tk,
qfoobar,rh,wl,tlr,&Q5);sbmi2oqk(qfoo,qbar,&Q5,Q3,q4,qfOBAz,
qfoobaz,QQUUX);}
