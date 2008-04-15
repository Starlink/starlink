/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmxy2xy(double qfoo,double qbar,double qbaz[6],double*
Q0,double*qfobar){*Q0=qbaz[0]+qbaz[1]*qfoo+qbaz[2]*qbar;*
qfobar=qbaz[3]+qbaz[4]*qfoo+qbaz[5]*qbar;}
