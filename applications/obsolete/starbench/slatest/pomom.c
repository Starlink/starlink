/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmpomom(double qfoo,double qbar,double qbaz,double Q0[
3][3]){sbmdeuler("\172\171\170",sbmsp(qfoo),-qbar,-qbaz,Q0);
}
