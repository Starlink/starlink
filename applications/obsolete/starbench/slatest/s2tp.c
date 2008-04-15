/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbms2tp(float qfoo,float qbar,float qbaz,float Q0,float
*qfobar,float*q1,int*q2)
#define qfoobar 1e-6f
{float Q3,q4,qfOBAz,qfoobaz,QQUUX,Q5,QFRED,qdog;Q3=(float)
sin(Q0);q4=(float)sin(qbar);qfOBAz=(float)cos(Q0);qfoobaz=(
float)cos(qbar);QQUUX=qfoo-qbaz;Q5=(float)sin(QQUUX);QFRED=(
float)cos(QQUUX);qdog=q4*Q3+qfoobaz*qfOBAz*QFRED;if(qdog>
qfoobar){*q2=0;}else if(qdog>=0.0f){*q2=1;qdog=qfoobar;}else
 if(qdog>-qfoobar){*q2=2;qdog=-qfoobar;}else{*q2=3;}*qfobar=
qfoobaz*Q5/qdog;*q1=(q4*qfOBAz-qfoobaz*Q3*QFRED)/qdog;}
