/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmcaldj(int qfoo,int qbar,int qbaz,double*Q0,int*
qfobar){int q1;if((qfoo>=0)&&(qfoo<=49))q1=qfoo+2000;else if
((qfoo>=50)&&(qfoo<=99))q1=qfoo+1900;else q1=qfoo;sbmcldj(q1
,qbar,qbaz,Q0,qfobar);}
