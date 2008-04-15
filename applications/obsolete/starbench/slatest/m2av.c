/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmm2av(float qfoo[3][3],float qbar[3]){float qbaz,Q0,
qfobar,q1,q2,phi,qfoobar;qbaz=qfoo[1][2]-qfoo[2][1];Q0=qfoo[
2][0]-qfoo[0][2];qfobar=qfoo[0][1]-qfoo[1][0];q1=(float)sqrt
((double)(qbaz*qbaz+Q0*Q0+qfobar*qfobar));if(q1!=0.0f){q2=
qfoo[0][0]+qfoo[1][1]+qfoo[2][2]-1.0f;phi=(float)atan2((
double)q1/2.0,(double)q2/2.0);qfoobar=phi/q1;qbar[0]=qbaz*
qfoobar;qbar[1]=Q0*qfoobar;qbar[2]=qfobar*qfoobar;}else{qbar
[0]=0.0f;qbar[1]=0.0f;qbar[2]=0.0f;}}
