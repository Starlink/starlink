/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmcc2s(float qfoo[3],float*qbar,float*qbaz){double Q0,
qfobar,q1,q2;Q0=(double)qfoo[0];qfobar=(double)qfoo[1];q1=(
double)qfoo[2];q2=sqrt(Q0*Q0+qfobar*qfobar);*qbar=(q2!=0.0)?
(float)atan2(qfobar,Q0):0.0f;*qbaz=(q1!=0.0)?(float)atan2(q1
,q2):0.0f;}
