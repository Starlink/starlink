/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdtf2r(int qfoo,int qbar,double qbaz,double*Q0,int*
qfobar){double q1;sbmdtf2d(qfoo,qbar,qbaz,&q1,qfobar);*Q0=
D2PI*q1;}
