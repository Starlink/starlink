/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdd2tf(int qfoo,double qbar,char*qbaz,int Q0[4])
#define qfobar 86400.0
{double q1,q2,rh,qfoobar,Q3,q4,qfOBAz,qfoobaz;*qbaz=(char)((
qbar<0.0)?'-':'+');q1=pow(10.0,(double)gmax(qfoo,0));q1=dint
(q1);q2=q1*60.0;rh=q2*60.0;qfoobar=q1*qfobar*fabs(qbar);
qfoobar=dnint(qfoobar);Q3=qfoobar/rh;Q3=dint(Q3);qfoobar=
qfoobar-Q3*rh;q4=qfoobar/q2;q4=dint(q4);qfoobar=qfoobar-q4*
q2;qfOBAz=qfoobar/q1;qfOBAz=dint(qfOBAz);qfoobaz=qfoobar-
qfOBAz*q1;Q0[0]=(int)Q3;Q0[1]=(int)q4;Q0[2]=(int)qfOBAz;Q0[3
]=(int)qfoobaz;}
