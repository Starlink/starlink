/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmcs2c(float qfoo,float qbar,float qbaz[3]){float Q0;
Q0=(float)cos(qbar);qbaz[0]=(float)cos(qfoo)*Q0;qbaz[1]=(
float)sin(qfoo)*Q0;qbaz[2]=(float)sin(qbar);}
