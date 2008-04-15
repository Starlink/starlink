/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmav2m(float qfoo[3],float qbar[3][3]){double qbaz,Q0,
qfobar,phi,q1,q2,qfoobar;qbaz=(double)qfoo[0];Q0=(double)
qfoo[1];qfobar=(double)qfoo[2];phi=sqrt(qbaz*qbaz+Q0*Q0+
qfobar*qfobar);q1=sin(phi);q2=cos(phi);qfoobar=1.0-q2;if(phi
!=0.0){qbaz=qbaz/phi;Q0=Q0/phi;qfobar=qfobar/phi;}qbar[0][0]
=(float)(qbaz*qbaz*qfoobar+q2);qbar[0][1]=(float)(qbaz*Q0*
qfoobar+qfobar*q1);qbar[0][2]=(float)(qbaz*qfobar*qfoobar-Q0
*q1);qbar[1][0]=(float)(qbaz*Q0*qfoobar-qfobar*q1);qbar[1][1
]=(float)(Q0*Q0*qfoobar+q2);qbar[1][2]=(float)(Q0*qfobar*
qfoobar+qbaz*q1);qbar[2][0]=(float)(qbaz*qfobar*qfoobar+Q0*
q1);qbar[2][1]=(float)(Q0*qfobar*qfoobar-qbaz*q1);qbar[2][2]
=(float)(qfobar*qfobar*qfoobar+q2);}
