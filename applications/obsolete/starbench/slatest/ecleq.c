/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmecleq(double qfoo,double qbar,double qbaz,double*Q0,
double*qfobar){double q1[3][3],q2[3],qfoobar[3];sbmdcs2c(
qfoo,qbar,q2);sbmecmat(qbaz,q1);sbmdimxv(q1,q2,qfoobar);
sbmprec(2000.0,sbmepj(qbaz),q1);sbmdimxv(q1,qfoobar,q2);
sbmdcc2s(q2,Q0,qfobar);*Q0=sbmdranrm(*Q0);*qfobar=sbmdrange(
*qfobar);}
