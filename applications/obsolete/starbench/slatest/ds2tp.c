/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmds2tp(double qfoo,double qbar,double qbaz,double Q0,
double*qfobar,double*q1,int*q2)
#define qfoobar 1e-6
{double Q3,q4,qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog;Q3=sin(Q0);
q4=sin(qbar);qfOBAz=cos(Q0);qfoobaz=cos(qbar);QQUUX=qfoo-
qbaz;Q5=sin(QQUUX);QFRED=cos(QQUUX);qdog=q4*Q3+qfoobaz*
qfOBAz*QFRED;if(qdog>qfoobar){*q2=0;}else if(qdog>=0.0){*q2=
1;qdog=qfoobar;}else if(qdog>-qfoobar){*q2=2;qdog=-qfoobar;}
else{*q2=3;}*qfobar=qfoobaz*Q5/qdog;*q1=(q4*qfOBAz-qfoobaz*
Q3*QFRED)/qdog;}
