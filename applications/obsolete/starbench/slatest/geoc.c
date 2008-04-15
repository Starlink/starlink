/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmgeoc(double p,double qfoo,double*qbar,double*qbaz){
double Q0,qfobar,q1,q2;static double qfoobar=6378140.0;
static double Q3=1.0/298.257;double q4=(1.0-Q3)*(1.0-Q3);
static double qfOBAz=1.49597870e11;Q0=sin(p);qfobar=cos(p);
q1=1.0/sqrt(qfobar*qfobar+q4*Q0*Q0);q2=q4*q1;*qbar=(qfoobar*
q1+qfoo)*qfobar/qfOBAz;*qbaz=(qfoobar*q2+qfoo)*Q0/qfOBAz;}
