/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
double sbmeors(double qfoo[3][3],double qbar){double qbaz,Q0
,qfobar,q1,q2,p,qfoobar;qbaz=qfoo[2][0];Q0=qbaz/(1.0+qfoo[2]
[2]);qfobar=1.0-Q0*qbaz;q1=-Q0*qfoo[2][1];q2=-qbaz;p=qfoo[0]
[0]*qfobar+qfoo[0][1]*q1+qfoo[0][2]*q2;qfoobar=qfoo[1][0]*
qfobar+qfoo[1][1]*q1+qfoo[1][2]*q2;return qbar-((p!=0.0||
qfoobar!=0.0)?atan2(qfoobar,p):0.0);}
