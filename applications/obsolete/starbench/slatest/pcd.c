/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
void sbmpcd(double qfoo,double*qbar,double*qbaz){double Q0;
Q0=1.0+qfoo*(*qbar**qbar+*qbaz**qbaz);*qbar*=Q0;*qbaz*=Q0;}
