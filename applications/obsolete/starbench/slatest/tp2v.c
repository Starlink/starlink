/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmtp2v(float qfoo,float qbar,float qbaz[3],float Q0[3]
){float qfobar,q1,q2,qfoobar,Q3;qfobar=qbaz[0];q1=qbaz[1];q2
=qbaz[2];qfoobar=(float)sqrt((double)(1.0f+qfoo*qfoo+qbar*
qbar));Q3=(float)sqrt((double)(qfobar*qfobar+q1*q1));if(Q3==
0.0f){Q3=1e-20f;qfobar=Q3;}Q0[0]=(qfobar-(qfoo*q1+qbar*
qfobar*q2)/Q3)/qfoobar;Q0[1]=(q1+(qfoo*qfobar-qbar*q1*q2)/Q3
)/qfoobar;Q0[2]=(q2+qbar*Q3)/qfoobar;}
