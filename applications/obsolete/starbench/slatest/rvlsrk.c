/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
float sbmrvlsrk(float qfoo,float qbar){static float qbaz[3]=
{-0.29000f,17.31726f,-10.00141f};float Q0[3];sbmcs2c(qfoo,
qbar,Q0);return sbmvdv(qbaz,Q0);}
