/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdtp2v(double qfoo,double qbar,double qbaz[3],double
Q0[3]){double qfobar,q1,q2,qfoobar,Q3;qfobar=qbaz[0];q1=qbaz
[1];q2=qbaz[2];qfoobar=sqrt(1.0+qfoo*qfoo+qbar*qbar);Q3=sqrt
(qfobar*qfobar+q1*q1);if(Q3==0.0){Q3=1e-20;qfobar=Q3;}Q0[0]=
(qfobar-(qfoo*q1+qbar*qfobar*q2)/Q3)/qfoobar;Q0[1]=(q1+(qfoo
*qfobar-qbar*q1*q2)/Q3)/qfoobar;Q0[2]=(q2+qbar*Q3)/qfoobar;}
