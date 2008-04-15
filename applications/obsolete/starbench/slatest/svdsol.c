/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmsvdsol(int qfoo,int qbar,int qbaz,int Q0,double*
qfobar,double*q1,double*q2,double*qfoobar,double*Q3,double*
q4){int qfOBAz,qfoobaz,QQUUX;double Q5;double*QFRED;double*
qdog;if(qfoo>=qbar&&qfoo<=qbaz&&qbar<=Q0){for(qfOBAz=0;
qfOBAz<qbar;qfOBAz++){Q5=0.0;if(q2[qfOBAz]!=0.0){for(qfoobaz
=0,QFRED=q1;qfoobaz<qfoo;qfoobaz++,QFRED+=Q0){Q5+=QFRED[
qfOBAz]*qfobar[qfoobaz];}Q5/=q2[qfOBAz];}Q3[qfOBAz]=Q5;}for(
qfOBAz=0,qdog=qfoobar;qfOBAz<qbar;qfOBAz++,qdog+=Q0){Q5=0.0;
for(QQUUX=0;QQUUX<qbar;QQUUX++){Q5+=qdog[QQUUX]*Q3[QQUUX];}
q4[qfOBAz]=Q5;}}}
