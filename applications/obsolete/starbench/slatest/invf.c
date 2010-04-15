/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbminvf(double qfoo[6],double qbar[6],int*qbaz){double
Q0,qfobar,q1,q2,qfoobar,Q3,q4;Q0=qfoo[0];qfobar=qfoo[1];q1=
qfoo[2];q2=qfoo[3];qfoobar=qfoo[4];Q3=qfoo[5];q4=qfobar*Q3-
q1*qfoobar;if(q4!=0.0){qbar[0]=(q1*q2-Q0*Q3)/q4;qbar[1]=Q3/
q4;qbar[2]=-q1/q4;qbar[3]=(Q0*qfoobar-qfobar*q2)/q4;qbar[4]=
-qfoobar/q4;qbar[5]=qfobar/q4;*qbaz=0;}else{*qbaz=-1;}}
