/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdaf2r(int qfoo,int qbar,double qbaz,double*Q0,int*
qfobar){int q1;q1=0;if((qbaz<0.0)||(qbaz>=60.0))q1=3;if((
qbar<0)||(qbar>59))q1=2;if((qfoo<0)||(qfoo>359))q1=1;*Q0=
DAS2R*(60.0*(60.0*(double)qfoo+(double)qbar)+qbaz);*qfobar=
q1;}
