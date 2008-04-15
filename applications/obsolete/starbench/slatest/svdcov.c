/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmsvdcov(int qfoo,int qbar,int qbaz,double*Q0,double*
qfobar,double*q1,double*q2){int qfoobar,Q3,q4;double qfOBAz;
double*qfoobaz,*QQUUX;double*Q5,*QFRED;for(qfoobar=0;qfoobar
<qfoo;qfoobar++){qfOBAz=Q0[qfoobar];if(qfOBAz!=0.0)q1[
qfoobar]=1.0/(qfOBAz*qfOBAz);else q1[qfoobar]=0.0;}for(
qfoobar=0,qfoobaz=qfobar,Q5=q2;qfoobar<qfoo;qfoobar++,
qfoobaz+=qbar,Q5+=qbaz){for(Q3=0,QQUUX=qfobar,QFRED=q2;Q3<=
qfoobar;Q3++,QQUUX+=qbar,QFRED+=qbaz){qfOBAz=0.0;for(q4=0;q4
<qfoo;q4++){qfOBAz+=qfoobaz[q4]*QQUUX[q4]*q1[q4];}Q5[Q3]=
qfOBAz;QFRED[qfoobar]=qfOBAz;}}}
