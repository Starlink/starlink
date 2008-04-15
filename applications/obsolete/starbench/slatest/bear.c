/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
float sbmbear(float qfoo,float qbar,float qbaz,float Q0){
return(float)sbmdbear((double)qfoo,(double)qbar,(double)qbaz
,(double)Q0);}
