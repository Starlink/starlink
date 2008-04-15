/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmprenut(double qfoo,double qbar,double qbaz[3][3]){
double Q0[3][3],qfobar[3][3];sbmprec(qfoo,sbmepj(qbar),Q0);
sbmnut(qbar,qfobar);sbmdmxm(qfobar,Q0,qbaz);}
