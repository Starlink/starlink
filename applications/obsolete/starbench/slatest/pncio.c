/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmpncio(double qfoo,double qbar[3][3]){double qbaz,Q0,
qfobar,q1,q2,qfoobar,Q3,q4;sbmpfw(qfoo,&qbaz,&Q0,&qfobar,&q1
);sbmnu(qfoo,&q2,&qfoobar);sbmfw2xy(qbaz,Q0,qfobar+q2,q1+
qfoobar,&Q3,&q4);sbmg2ixys(Q3,q4,sbms(qfoo,Q3,q4),qbar);}
