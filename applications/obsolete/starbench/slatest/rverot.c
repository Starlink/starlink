/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
float sbmrverot(float phi,float qfoo,float qbar,float qbaz)
#define Q0 0.4655
{return(float)(Q0*cos((double)phi)*sin((double)(qbaz-qfoo))*
cos((double)qbar));}
