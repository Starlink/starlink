/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmh2e(float qfoo,float qbar,float phi,float*qbaz,float
*Q0){float qfobar,q1,q2,qfoobar,Q3,q4,qfOBAz,qfoobaz,QQUUX,
Q5;qfobar=(float)sin(qfoo);q1=(float)cos(qfoo);q2=(float)sin
(qbar);qfoobar=(float)cos(qbar);Q3=(float)sin(phi);q4=(float
)cos(phi);qfOBAz=-q1*qfoobar*Q3+q2*q4;qfoobaz=-qfobar*
qfoobar;QQUUX=q1*qfoobar*q4+q2*Q3;Q5=(float)sqrt(qfOBAz*
qfOBAz+qfoobaz*qfoobaz);*qbaz=(Q5!=0.0f)?(float)atan2(
qfoobaz,qfOBAz):0.0f;*Q0=(float)atan2(QQUUX,Q5);}
