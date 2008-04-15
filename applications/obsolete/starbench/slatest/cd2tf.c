/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmcd2tf(int qfoo,float qbar,char*qbaz,int Q0[4]){
sbmdd2tf(qfoo,(double)qbar,qbaz,Q0);}
