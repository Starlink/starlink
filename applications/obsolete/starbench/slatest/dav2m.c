/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdav2m(double qfoo[3],double qbar[3][3]){double qbaz,
Q0,qfobar,phi,q1,q2,qfoobar;qbaz=qfoo[0];Q0=qfoo[1];qfobar=
qfoo[2];phi=sqrt(qbaz*qbaz+Q0*Q0+qfobar*qfobar);q1=sin(phi);
q2=cos(phi);qfoobar=1.0-q2;if(phi!=0.0){qbaz=qbaz/phi;Q0=Q0/
phi;qfobar=qfobar/phi;}qbar[0][0]=qbaz*qbaz*qfoobar+q2;qbar[
0][1]=qbaz*Q0*qfoobar+qfobar*q1;qbar[0][2]=qbaz*qfobar*
qfoobar-Q0*q1;qbar[1][0]=qbaz*Q0*qfoobar-qfobar*q1;qbar[1][1
]=Q0*Q0*qfoobar+q2;qbar[1][2]=Q0*qfobar*qfoobar+qbaz*q1;qbar
[2][0]=qbaz*qfobar*qfoobar+Q0*q1;qbar[2][1]=Q0*qfobar*
qfoobar-qbaz*q1;qbar[2][2]=qfobar*qfobar*qfoobar+q2;}
