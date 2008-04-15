/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdcc2s(double qfoo[3],double*qbar,double*qbaz){double
 Q0,qfobar,q1,q2;Q0=qfoo[0];qfobar=qfoo[1];q1=qfoo[2];q2=
sqrt(Q0*Q0+qfobar*qfobar);*qbar=(q2!=0.0)?atan2(qfobar,Q0):
0.0;*qbaz=(q1!=0.0)?atan2(q1,q2):0.0;}
