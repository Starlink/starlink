/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
double sbmgmst(double qfoo){double qbar;qbar=(qfoo-51544.5)/
36525.0;return sbmdranrm(dmod(qfoo,1.0)*D2PI+(24110.54841+(
8640184.812866+(0.093104-6.2e-6*qbar)*qbar)*qbar)*DS2R);}
