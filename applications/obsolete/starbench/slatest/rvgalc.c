/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
float sbmrvgalc(float qfoo,float qbar){static float qbaz[3]=
{-108.70408f,97.86251f,-164.33610f};float Q0[3];sbmcs2c(qfoo
,qbar,Q0);return sbmvdv(qbaz,Q0);}
