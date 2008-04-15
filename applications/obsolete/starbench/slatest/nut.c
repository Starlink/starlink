/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmnut(double qfoo,double qbar[3][3]){double qbaz,Q0,
qfobar;sbmnutc(qfoo,&qbaz,&Q0,&qfobar);sbmdeuler(
"\170\172\170",qfobar,-qbaz,-(qfobar+Q0),qbar);}
