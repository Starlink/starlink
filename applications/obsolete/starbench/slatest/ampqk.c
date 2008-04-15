/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmampqk(double qfoo,double qbar,double qbaz[21],double
*Q0,double*qfobar){double gr2e;double ab1;double ehn[3];
double abv[3];double p[3],q1[3],q2[3],qfoobar[3];double Q3,
q4,qfOBAz,qfoobaz,QQUUX,Q5;int QFRED,qdog;gr2e=qbaz[7];ab1=
qbaz[11];for(QFRED=0;QFRED<3;QFRED++){ehn[QFRED]=qbaz[QFRED+
4];abv[QFRED]=qbaz[QFRED+8];}sbmdcs2c(qfoo,qbar,qfoobar);
sbmdimxv((double(*)[3])&qbaz[12],qfoobar,q2);Q3=ab1+1.0;for(
QFRED=0;QFRED<3;QFRED++){q1[QFRED]=q2[QFRED];}for(qdog=0;
qdog<2;qdog++){q4=sbmdvdv(q1,abv);qfOBAz=1.0+q4;qfoobaz=1.0+
q4/Q3;for(QFRED=0;QFRED<3;QFRED++){q1[QFRED]=(qfOBAz*q2[
QFRED]-qfoobaz*abv[QFRED])/ab1;}sbmdvn(q1,qfoobar,&qfoobaz);
for(QFRED=0;QFRED<3;QFRED++){q1[QFRED]=qfoobar[QFRED];}}for(
QFRED=0;QFRED<3;QFRED++){p[QFRED]=q1[QFRED];}for(qdog=0;qdog
<5;qdog++){QQUUX=sbmdvdv(p,ehn);Q5=1.0+QQUUX;qfoobaz=Q5-gr2e
*QQUUX;for(QFRED=0;QFRED<3;QFRED++){p[QFRED]=(Q5*q1[QFRED]-
gr2e*ehn[QFRED])/qfoobaz;}sbmdvn(p,q2,&qfoobaz);for(QFRED=0;
QFRED<3;QFRED++){p[QFRED]=q2[QFRED];}}sbmdcc2s(p,Q0,qfobar);
*Q0=sbmdranrm(*Q0);}
