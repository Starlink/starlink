/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmcr2af(int qfoo,float qbar,char*qbaz,int Q0[4]){
sbmdd2tf(qfoo,(double)qbar*D15B2P,qbaz,Q0);}
