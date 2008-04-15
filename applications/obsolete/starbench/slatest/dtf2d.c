/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdtf2d(int qfoo,int qbar,double qbaz,double*Q0,int*
qfobar)
#define q1 86400.0
{int q2;q2=0;if((qbaz<0.0)||(qbaz>=60.0))q2=3;if((qbar<0)||(
qbar>59))q2=2;if((qfoo<0)||(qfoo>23))q2=1;*Q0=(60.0*(60.0*(
double)qfoo+(double)qbar)+qbaz)/q1;*qfobar=q2;}
