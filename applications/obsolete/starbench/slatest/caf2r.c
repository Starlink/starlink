/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmcaf2r(int qfoo,int qbar,float qbaz,float*Q0,int*
qfobar){double q1;sbmdaf2r(qfoo,qbar,(double)qbaz,&q1,qfobar
);*Q0=(float)q1;}
