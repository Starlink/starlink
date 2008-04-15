/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmi2cqk(double qfoo,double qbar,CIpars*qbaz,double*Q0,
double*qfobar){int q1,q2;double gr2e,ab1,ehn[3],abv[3],
qfoobar[3],Q3[3],q4,qfOBAz,qfoobaz,QQUUX[3],Q5,QFRED,qdog,p[
3];gr2e=qbaz->gr2e;ab1=qbaz->ab1;for(q1=0;q1<3;q1++){ehn[q1]
=qbaz->ehn[q1];abv[q1]=qbaz->abv[q1];}sbmdcs2c(qfoo,qbar,
qfoobar);sbmdimxv(qbaz->bpn,qfoobar,Q3);q4=ab1+1.0;for(q1=0;
q1<3;q1++){QQUUX[q1]=Q3[q1];}for(q2=0;q2<2;q2++){qfOBAz=
sbmdvdv(QQUUX,abv);qfoobaz=1.0+qfOBAz;Q5=1.0+qfOBAz/q4;for(
q1=0;q1<3;q1++){QQUUX[q1]=(qfoobaz*Q3[q1]-Q5*abv[q1])/ab1;}
sbmdvn(QQUUX,qfoobar,&Q5);for(q1=0;q1<3;q1++){QQUUX[q1]=
qfoobar[q1];}}for(q1=0;q1<3;q1++){p[q1]=QQUUX[q1];}for(q2=0;
q2<5;q2++){QFRED=sbmdvdv(p,ehn);qdog=1.0+QFRED;Q5=qdog-gr2e*
QFRED;for(q1=0;q1<3;q1++){p[q1]=(qdog*QQUUX[q1]-gr2e*ehn[q1]
)/Q5;}sbmdvn(p,Q3,&Q5);for(q1=0;q1<3;q1++){p[q1]=Q3[q1];}}
sbmdcc2s(p,Q0,qfobar);*Q0=sbmdranrm(*Q0);}
