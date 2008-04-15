/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmplanel(double qfoo,int qbar,double qbaz,double Q0,
double qfobar,double q1,double q2,double qfoobar,double Q3,
double q4,double qfOBAz[6],int*qfoobaz){double QQUUX[13];int
 Q5;sbmel2ue(qfoo,qbar,qbaz,Q0,qfobar,q1,q2,qfoobar,Q3,q4,
QQUUX,&Q5);if(!Q5){sbmue2pv(qfoo,QQUUX,qfOBAz,&Q5);if(Q5)Q5=
-5;}*qfoobaz=Q5;}
