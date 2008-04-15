/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmo2i(char*qfoo,double qbar,double qbaz,double Q0,
double qfobar,double q1,double phi,double hm,double q2,
double qfoobar,double tk,double Q3,double rh,double wl,
double tlr,double*q4,double*qfOBAz){IOpars qfoobaz;sbmi2opa(
Q0,qfobar,q1,phi,hm,q2,qfoobar,tk,Q3,rh,wl,tlr,&qfoobaz);
sbmo2iqk(qfoo,qbar,qbaz,&qfoobaz,q4,qfOBAz);}
