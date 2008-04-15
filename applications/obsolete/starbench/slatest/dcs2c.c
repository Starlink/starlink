/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdcs2c(double qfoo,double qbar,double qbaz[3]){double
 Q0;Q0=cos(qbar);qbaz[0]=cos(qfoo)*Q0;qbaz[1]=sin(qfoo)*Q0;
qbaz[2]=sin(qbar);}
