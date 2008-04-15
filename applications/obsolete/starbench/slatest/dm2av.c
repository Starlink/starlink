/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdm2av(double qfoo[3][3],double qbar[3]){double qbaz,
Q0,qfobar,q1,q2,phi,qfoobar;qbaz=qfoo[1][2]-qfoo[2][1];Q0=
qfoo[2][0]-qfoo[0][2];qfobar=qfoo[0][1]-qfoo[1][0];q1=sqrt(
qbaz*qbaz+Q0*Q0+qfobar*qfobar);if(q1!=0.0){q2=qfoo[0][0]+
qfoo[1][1]+qfoo[2][2]-1.0;phi=atan2(q1,q2);qfoobar=phi/q1;
qbar[0]=qbaz*qfoobar;qbar[1]=Q0*qfoobar;qbar[2]=qfobar*
qfoobar;}else{qbar[0]=0.0;qbar[1]=0.0;qbar[2]=0.0;}}
