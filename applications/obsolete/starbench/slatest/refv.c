/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmrefv(double qfoo[3],double refa,double refb,double
qbar[3]){double qbaz,Q0,qfobar,q1,q2,qfoobar,Q3,q4,qfOBAz,
qfoobaz,QQUUX,Q5;qbaz=qfoo[0];Q0=qfoo[1];qfobar=qfoo[2];q1=
gmax(qfobar,0.05);q2=q1*q1;qfoobar=qbaz*qbaz+Q0*Q0;Q3=sqrt(
qfoobar);q4=refb*qfoobar/q2;qfOBAz=(refa+q4)/(1.0+(refa+3.0*
q4)*(q2+qfoobar)/q2);qfoobaz=qfOBAz*Q3/q1;QQUUX=1.0-qfoobaz*
qfoobaz/2.0;Q5=QQUUX*(1.0-qfOBAz);qbar[0]=qbaz*Q5;qbar[1]=Q0
*Q5;qbar[2]=QQUUX*(q1+qfoobaz*Q3)+(qfobar-q1);}
