/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
float sbmrvlg(float qfoo,float qbar){static float qbaz[3]={-
148.23284f,133.44888f,-224.09467f};float Q0[3];sbmcs2c(qfoo,
qbar,Q0);return sbmvdv(qbaz,Q0);}
