/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
float sbmrvlsrd(float qfoo,float qbar){static float qbaz[3]=
{0.63823f,14.58542f,-7.80116f};float Q0[3];sbmcs2c(qfoo,qbar
,Q0);return sbmvdv(qbaz,Q0);}
