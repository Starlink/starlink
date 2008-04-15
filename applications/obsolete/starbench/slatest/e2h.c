/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbme2h(float qfoo,float qbar,float phi,float*qbaz,float
*Q0){float qfobar,q1,q2,qfoobar,Q3,q4,qfOBAz,qfoobaz,QQUUX,
Q5,QFRED;qfobar=(float)sin(qfoo);q1=(float)cos(qfoo);q2=(
float)sin(qbar);qfoobar=(float)cos(qbar);Q3=(float)sin(phi);
q4=(float)cos(phi);qfOBAz=-q1*qfoobar*Q3+q2*q4;qfoobaz=-
qfobar*qfoobar;QQUUX=q1*qfoobar*q4+q2*Q3;Q5=(float)sqrt(
qfOBAz*qfOBAz+qfoobaz*qfoobaz);QFRED=(Q5!=0.0f)?(float)atan2
(qfoobaz,qfOBAz):0.0f;*qbaz=(QFRED<0.0f)?(float)((double)
QFRED+D2PI):QFRED;*Q0=(float)atan2(QQUUX,Q5);}
