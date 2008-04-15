/*
** Copyright (C) 2005 P.T.Wallace.
** Use for profit prohibited - enquiries to ptw@tpsoft.demon.co.uk.
*/
#include "sbmlib.h"
#include "sbmmac.h"
double sbmdt(double qfoo){double qbar,qbaz,Q0;qbar=(qfoo-
1800.0)/100.0;if(qfoo>=1708.185161980887){qbaz=qbar-0.19;Q0=
5.156+13.3066*qbaz*qbaz;}else{if(qfoo>=979.0258204760233){Q0
=25.5*qbar*qbar;}else{Q0=1360.0+(320.0+44.3*qbar)*qbar;}}
return Q0;}
