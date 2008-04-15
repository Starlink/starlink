/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmc2iqkz(double qfoo,double qbar,CIpars*qbaz,double*Q0
,double*qfobar){int q1;double p[3],q2,qfoobar,Q3,q4[3],
qfOBAz,qfoobaz[3],QQUUX[3];sbmdcs2c(qfoo,qbar,p);q2=sbmdvdv(
p,qbaz->ehn);qfoobar=q2+1.0;Q3=qbaz->gr2e/gmax(qfoobar,
1.0e-5);for(q1=0;q1<3;q1++){q4[q1]=p[q1]+Q3*(qbaz->ehn[q1]-
q2*p[q1]);}qfOBAz=sbmdvdv(q4,qbaz->abv);Q3=1.0+qfOBAz/(qbaz
->ab1+1.0);for(q1=0;q1<3;q1++){qfoobaz[q1]=qbaz->ab1*q4[q1]+
Q3*qbaz->abv[q1];}sbmdmxv(qbaz->bpn,qfoobaz,QQUUX);sbmdcc2s(
QQUUX,Q0,qfobar);*Q0=sbmdranrm(*Q0);}
