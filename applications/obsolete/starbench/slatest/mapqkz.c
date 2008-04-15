/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmmapqkz(double qfoo,double qbar,double qbaz[21],
double*Q0,double*qfobar){int q1;double gr2e,ab1,ehn[3],abv[3
],p[3],q2,qfoobar,Q3,q4[3],qfOBAz,qfoobaz,QQUUX[3],Q5[3];
gr2e=qbaz[7];ab1=qbaz[11];for(q1=0;q1<3;q1++){ehn[q1]=qbaz[
q1+4];abv[q1]=qbaz[q1+8];}sbmdcs2c(qfoo,qbar,p);q2=sbmdvdv(p
,ehn);qfoobar=q2+1.0;Q3=gr2e/gmax(qfoobar,1e-5);for(q1=0;q1<
3;q1++){q4[q1]=p[q1]+Q3*(ehn[q1]-q2*p[q1]);}qfOBAz=sbmdvdv(
q4,abv);qfoobaz=qfOBAz+1.0;Q3=1.0+qfOBAz/(ab1+1.0);for(q1=0;
q1<3;q1++){QQUUX[q1]=((ab1*q4[q1])+(Q3*abv[q1]))/qfoobaz;}
sbmdmxv((double(*)[3])&qbaz[12],QQUUX,Q5);sbmdcc2s(Q5,Q0,
qfobar);*Q0=sbmdranrm(*Q0);}
