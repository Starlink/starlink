/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmaddet(double qfoo,double qbar,double qbaz,double*Q0,
double*qfobar){double q1[3];double q2[3];int qfoobar;
sbmetrms(qbaz,q1);sbmdcs2c(qfoo,qbar,q2);for(qfoobar=0;
qfoobar<3;qfoobar++){q2[qfoobar]+=q1[qfoobar];}sbmdcc2s(q2,
Q0,qfobar);*Q0=sbmdranrm(*Q0);}
