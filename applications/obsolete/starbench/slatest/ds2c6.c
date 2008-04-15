/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmds2c6(double qfoo,double qbar,double qbaz,double Q0,
double qfobar,double q1,double q2[6]){double qfoobar,Q3,q4,
qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog;qfoobar=sin(qfoo);Q3=cos(
qfoo);q4=sin(qbar);qfOBAz=cos(qbar);qfoobaz=qbaz*qfOBAz;
QQUUX=qfoobaz*Q3;Q5=qfoobaz*qfoobar;QFRED=qbaz*qfobar;qdog=
QFRED*q4-qfOBAz*q1;q2[0]=QQUUX;q2[1]=Q5;q2[2]=qbaz*q4;q2[3]=
-Q5*Q0-qdog*Q3;q2[4]=QQUUX*Q0-qdog*qfoobar;q2[5]=QFRED*
qfOBAz+q4*q1;}
