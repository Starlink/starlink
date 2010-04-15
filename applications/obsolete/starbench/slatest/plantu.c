/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmplantu(double qfoo,double qbar,double phi,double
qbaz[],double*Q0,double*qfobar,double*q1,int*q2)
#define qfoobar 499.004782
{int Q3;double q4[3],qfOBAz[3],qfoobaz[6],QQUUX[6],Q5[6],
QFRED[3][3],qdog[6],qcat,QFISH[6],QgASp,Q6,q7,q8,QBAD;sbmepv
(qfoo,&qfoobaz[0],&qfoobaz[3],qfOBAz,q4);for(Q3=3;Q3<6;Q3++)
{qfoobaz[Q3]/=86400.0;}sbmue2pv(qfoo,qbaz,QQUUX,q2);for(Q3=0
;Q3<6;Q3++){Q5[Q3]=QQUUX[Q3]-qfoobaz[Q3];}sbmprenut(2000.0,
qfoo,QFRED);sbmdmxv(QFRED,Q5,qdog);sbmdmxv(QFRED,&Q5[3],&
qdog[3]);qcat=sbmgmst(qfoo-sbmdt(sbmepj(qfoo))/86400.0)+qbar
;sbmpvobs(phi,0.0,qcat,QFISH);for(Q3=0;Q3<6;Q3++){Q5[Q3]=
qdog[Q3]-QFISH[Q3];}QgASp=Q5[0];Q6=Q5[1];q7=Q5[2];q8=sqrt(
QgASp*QgASp+Q6*Q6+q7*q7);QBAD=qfoobar*q8;for(Q3=0;Q3<3;Q3++)
{Q5[Q3]=Q5[Q3]-QBAD*Q5[Q3+3];}sbmdcc2s(Q5,Q0,qfobar);*Q0=
sbmdranrm(*Q0);*q1=q8;}
