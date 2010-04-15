/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmcs2c6(float qfoo,float qbar,float qbaz,float Q0,
float qfobar,float q1,float q2[6]){double qfoobar,Q3;float
q4,qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog,qcat,QFISH;qfoobar=(
double)qfoo;Q3=(double)qbar;q4=(float)sin(qfoobar);qfOBAz=(
float)cos(qfoobar);qfoobaz=(float)sin(Q3);QQUUX=(float)cos(
Q3);Q5=qbaz*QQUUX;QFRED=Q5*qfOBAz;qdog=Q5*q4;qcat=qbaz*
qfobar;QFISH=qcat*qfoobaz-QQUUX*q1;q2[0]=QFRED;q2[1]=qdog;q2
[2]=qbaz*qfoobaz;q2[3]=-qdog*Q0-QFISH*qfOBAz;q2[4]=QFRED*Q0-
QFISH*q4;q2[5]=qcat*QQUUX+qfoobaz*q1;}
