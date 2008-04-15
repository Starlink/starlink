/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
double sbmdranrm(double qfoo){double qbar;qbar=dmod(qfoo,
D2PI);return(qbar>=0.0)?qbar:qbar+D2PI;}
