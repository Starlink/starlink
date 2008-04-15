/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmdr2tf(int qfoo,double qbar,char*qbaz,int Q0[4]){
sbmdd2tf(qfoo,qbar/D2PI,qbaz,Q0);}
