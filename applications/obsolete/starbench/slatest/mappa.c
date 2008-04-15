/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmmappa(double qfoo,double qbar,double qbaz[21])
#define Q0 499.004782
#define qfobar 1.974126e-8
{int q1;double q2[3],qfoobar[3],Q3[3],q4,qfOBAz[3],qfoobaz;
qbaz[0]=sbmepj(qbar)-qfoo;sbmevp(qbar,qfoo,q2,&qbaz[1],
qfoobar,Q3);sbmdvn(Q3,&qbaz[4],&q4);qbaz[7]=qfobar/q4;for(q1
=0;q1<3;q1++){qbaz[q1+8]=q2[q1]*Q0;}sbmdvn(&qbaz[8],qfOBAz,&
qfoobaz);qbaz[11]=sqrt(1.0-qfoobaz*qfoobaz);sbmprenut(qfoo,
qbar,(double(*)[3])&qbaz[12]);}
