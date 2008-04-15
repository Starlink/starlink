/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmsubet(double qfoo,double qbar,double qbaz,double*Q0,
double*qfobar){double q1[3],q2[3],qfoobar;int Q3;sbmetrms(
qbaz,q1);sbmdcs2c(qfoo,qbar,q2);qfoobar=1.0+sbmdvdv(q2,q1);
for(Q3=0;Q3<3;Q3++){q2[Q3]=qfoobar*q2[Q3]-q1[Q3];}sbmdcc2s(
q2,Q0,qfobar);*Q0=sbmdranrm(*Q0);}
