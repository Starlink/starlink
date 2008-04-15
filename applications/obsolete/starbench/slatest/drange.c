/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
double sbmdrange(double qfoo){double qbar;qbar=dmod(qfoo,
D2PI);return(fabs(qbar)<DPI)?qbar:qbar-dsign(D2PI,qfoo);}
